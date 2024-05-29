use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use super::types::TypeSet;

type Wrapper<T> = Rc<RefCell<T>>;

type SignalWrapper = Wrapper<Signal>;

fn wrap<T>(data: T) -> Wrapper<T> {
    Rc::new(RefCell::new(data))
}

#[derive(Debug, Clone)]
pub struct Signal {
    value: RefCell<Option<TypeSet>>,
    clean: RefCell<bool>,
    updater: Updater,
    listeners: Wrapper<Vec<SignalWrapper>>,
}

impl Signal {
    pub fn new(initial_value: TypeSet) -> SignalWrapper {
        wrap(Signal {
            value: RefCell::new(Some(initial_value)),
            clean: RefCell::new(true),
            updater: Updater::Bottom,
            listeners: wrap(Vec::new()),
        })
    }

    pub fn from_signals(sources: Vec<&SignalWrapper>, rule: UpdateRule) -> SignalWrapper {
        let signal = wrap(Signal {
            value: RefCell::new(None),
            clean: RefCell::new(false),
            updater: Updater::Computed(
                wrap(sources.iter().map(|&source| source.clone()).collect()),
                rule,
            ),
            listeners: wrap(Vec::new()),
        });

        for source in sources {
            source.borrow().add_listener(signal.clone());
        }

        signal
    }

    fn inner_get(&self) -> Ref<TypeSet> {
        return Ref::map(self.value.borrow(), |opt| {
            opt.as_ref().expect("Value was None")
        });
    }

    pub fn get(&self) -> Ref<TypeSet> {
        if *self.clean.borrow() {
            return self.inner_get();
        }

        let new_value = self.updater.apply_rule();

        self.set(new_value);
        self.mark_clean();

        self.inner_get()
    }

    fn set(&self, new_value: TypeSet) {
        *self.value.borrow_mut() = Some(new_value);

        for listener in self.listeners.borrow().iter() {
            listener.borrow_mut().mark_dirty();
        }
    }

    fn mark_dirty(&self) {
        *self.clean.borrow_mut() = false;

        for listener in self.listeners.borrow().iter() {
            listener.borrow().mark_dirty();
        }
    }

    fn mark_clean(&self) {
        *self.clean.borrow_mut() = true;
    }

    fn add_listener(&self, child: SignalWrapper) {
        self.listeners.borrow_mut().push(child);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UpdateRule {
    Union,
    Intersection,
    Subtract,
}

#[derive(Debug, Clone)]
enum Updater {
    Bottom,
    Computed(Wrapper<Vec<SignalWrapper>>, UpdateRule),
}

impl Updater {
    fn apply_rule(&self) -> TypeSet {
        match &self {
            Updater::Bottom => unreachable!(),
            Updater::Computed(sources, rule) => match rule {
                UpdateRule::Union => Updater::union(sources),
                UpdateRule::Intersection => Updater::intersection(sources),
                UpdateRule::Subtract => Updater::subtract(sources),
            },
        }
    }

    fn union(sources: &Wrapper<Vec<SignalWrapper>>) -> TypeSet {
        let mut new_value = TypeSet::default();

        for source in sources.borrow().iter() {
            new_value.union(&source.borrow().get());
        }

        new_value
    }

    fn intersection(sources: &Wrapper<Vec<SignalWrapper>>) -> TypeSet {
        let sources = sources.borrow();

        let mut first_var_type = sources
            .first()
            .expect("Intersection needs at least one signal")
            .borrow()
            .get()
            .clone();

        for source in &sources[1..] {
            first_var_type.intersection(&source.borrow().get());
        }

        first_var_type
    }

    fn subtract(sources: &Wrapper<Vec<SignalWrapper>>) -> TypeSet {
        let sources = sources.borrow();
        let [left, right] = sources.as_slice() else {
            panic!("Expected subtract to be used with two signals");
        };

        let mut left_copy = left.borrow().get().clone();
        left_copy.difference(&right.borrow().get());

        left_copy
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        signals::{Signal, UpdateRule},
        types::{Primitive, Type, TypeSet},
    };

    #[test]
    fn test_signals() {
        // let x;
        // if (Math.random() < 0.5) {
        //     x = "string";
        // } else {
        //     x = 1;
        // }
        //
        // console.log(x);
        //          ^? string | number

        let x_string = Signal::new(TypeSet::new(Primitive::String.into()));
        let x_number = Signal::new(TypeSet::new(Primitive::Number.into()));

        let x_derived = Signal::from_signals(vec![&x_string, &x_number], UpdateRule::Union);

        println!("{:#?}", &x_derived.borrow().get());

        let x_intersect =
            Signal::from_signals(vec![&x_derived, &x_number], UpdateRule::Intersection);

        println!("{:#?}", &x_intersect.borrow().get());

        let x_subtract = Signal::from_signals(vec![&x_derived, &x_number], UpdateRule::Subtract);

        println!("{:#?}", &x_subtract.borrow().get());

        let x_string = Signal::new(TypeSet::new(Type::Primitive(Primitive::String)));
        let x_number = Signal::new(TypeSet::new(Primitive::Number.into()));
        let x_boolean = Signal::new(TypeSet::new(Primitive::Boolean.into()));

        let x_union = Signal::from_signals(vec![&x_string, &x_number], UpdateRule::Union);
        let x_intersect =
            Signal::from_signals(vec![&x_union, &x_boolean], UpdateRule::Intersection);
        let x_subtract = Signal::from_signals(vec![&x_union, &x_string], UpdateRule::Subtract);

        let x_union_2 = Signal::from_signals(vec![&x_union, &x_boolean], UpdateRule::Union);
        let x_intersect_2 =
            Signal::from_signals(vec![&x_union_2, &x_union], UpdateRule::Intersection);
        let x_subtract_2 = Signal::from_signals(vec![&x_union_2, &x_union], UpdateRule::Subtract);

        println!("x_union: {:?}", &x_union.borrow().get());
        println!("x_intersect: {:?}", &x_intersect.borrow().get());
        println!("x_subtract: {:?}", &x_subtract.borrow().get());

        println!("x_union_2: {:?}", &x_union_2.borrow().get());
        println!("x_intersect_2: {:?}", &x_intersect_2.borrow().get());
        println!("x_subtract_2: {:?}", &x_subtract_2.borrow().get());
    }
}
