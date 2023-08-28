use super::Environment;
use super::Instance;
use super::Value;

use std::cell::Cell;
use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ptr::NonNull;

#[derive(Debug, Clone, PartialEq)]
pub struct GcBox {
    mark: Cell<bool>,
    instance: Instance,
    next: Option<Gc>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) struct Gc {
    obj: NonNull<GcBox>,
    _marker: PhantomData<GcBox>,
}

impl Gc {
    fn obj(&self) -> &GcBox {
        unsafe { self.obj.as_ref() }
    }

    fn obj_mut(&mut self) -> &mut GcBox {
        unsafe { self.obj.as_mut() }
    }
}

impl fmt::Debug for Gc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.obj().instance.fmt(f)
    }
}

impl Deref for Gc {
    type Target = Instance;

    fn deref(&self) -> &Self::Target {
        &self.obj().instance
    }
}

impl DerefMut for Gc {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.obj.as_mut().instance }
    }
}

impl Value {
    fn mark(&self) {
        match self {
            Value::Function(function) => function.closure.environment.borrow().mark(),
            Value::Instance(instance) => instance.obj().mark.set(true),
            Value::Nil
            | Value::String(_)
            | Value::Number(_)
            | Value::Bool(_)
            | Value::Native(_)
            | Value::Class(_) => {}
        }
    }
}

impl Environment {
    fn mark(&self) {
        if self.mark.get() {
            return;
        }
        self.mark.set(true);
        for value in self.values.values() {
            value.mark();
        }

        for env in &self.sub {
            if let Some(env) = env.upgrade() {
                env.borrow().mark();
            }
        }
    }
}

#[derive(Debug)]
pub struct Collector {
    allocations: usize,
    objects: Option<Gc>,
}

impl Collector {
    pub(super) fn new() -> Collector {
        Collector {
            allocations: 0,
            objects: None,
        }
    }

    pub(super) fn allocate(&mut self, instance: Instance, globals: &Environment) -> Gc {
        if self.allocations >= 2048 {
            self.mark_sweep(globals);
        }
        let obj = NonNull::from(Box::leak(Box::new(GcBox {
            mark: Cell::new(false),
            instance,
            next: self.objects,
        })));
        self.allocations += 1;
        let gc = Gc {
            obj,
            _marker: PhantomData,
        };
        self.objects = Some(gc);
        gc
    }

    fn mark_sweep(&mut self, globals: &Environment) {
        globals.mark();
        self.collect();
    }

    pub(super) fn collect(&mut self) {
        let mut previous = None;
        let mut obj = self.objects;
        while let Some(gc) = obj {
            if gc.obj().mark.get() {
                gc.obj().mark.set(false);
                previous = obj;
                obj = gc.obj().next;
            } else {
                obj = gc.obj().next;
                if let Some(previous) = previous.as_mut() {
                    previous.obj_mut().next = obj;
                } else {
                    self.objects = obj;
                }

                let _ = unsafe { Box::from_raw(gc.obj.as_ptr()) };
            }
        }
    }
}
