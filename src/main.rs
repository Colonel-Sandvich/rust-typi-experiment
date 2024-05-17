use std::cell::RefCell;
use std::rc::Rc;
use std::vec::Vec;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum Type {
    Any,
    Number,
    String,
    Boolean,
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
struct VariableType {
    value: Vec<Type>,
}

impl VariableType {
    fn new(initial_value: Type) -> VariableType {
        VariableType {
            value: vec![initial_value],
        }
    }

    fn add_type(&mut self, add: Type) {
        if !self.value.contains(&add) {
            self.value.push(add);
        }
    }

    fn union(&mut self, other: &VariableType) {
        for t in &other.value {
            self.add_type(t.clone());
        }
    }
}

type Wrapper<T> = Rc<RefCell<T>>;

type SignalWrapper = Wrapper<Signal>;

fn wrap<T>(data: T) -> Wrapper<T> {
    Rc::new(RefCell::new(data))
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum UpdateRule {
    Union,
}

#[derive(Debug, Clone)]
enum Updater {
    Bottom,
    Computed(Wrapper<Vec<SignalWrapper>>, UpdateRule),
}

#[derive(Debug, Clone)]
struct Signal {
    value: Option<VariableType>,
    clean: bool,
    updater: Updater,
    listeners: Wrapper<Vec<SignalWrapper>>,
}

impl Signal {
    fn add_listener(&mut self, child: SignalWrapper) {
        self.listeners.borrow_mut().push(child.clone());
    }

    fn new(initial_value: VariableType) -> Signal {
        Signal {
            value: Some(initial_value),
            clean: true,
            updater: Updater::Bottom,
            listeners: wrap(Vec::new()),
        }
    }

    fn from_signals(sources: &Wrapper<Vec<SignalWrapper>>, rule: UpdateRule) -> SignalWrapper {
        let signal = wrap(Signal {
            value: None,
            clean: false,
            updater: Updater::Computed(sources.clone(), rule),
            listeners: wrap(Vec::new()),
        });

        for source in sources.borrow().iter() {
            source.borrow_mut().add_listener(signal.clone());
        }

        signal
    }

    fn inner_get(&mut self) -> &VariableType {
        self.value.as_ref().expect("Signal value is None")
    }

    fn get(&mut self) -> &VariableType {
        if self.clean {
            return self.inner_get();
        }

        match &self.updater {
            Updater::Bottom => return self.inner_get(),
            Updater::Computed(sources, rule) => {
                let mut new_value = VariableType::default();

                for source in sources.borrow().iter() {
                    match rule {
                        UpdateRule::Union => {
                            new_value.union(&source.borrow_mut().get());
                        }
                    }
                }

                self.set(new_value);
                self.clean = true;
                self.inner_get()
            }
        }
    }

    fn set(&mut self, new_value: VariableType) {
        self.value = Some(new_value);

        for listener in self.listeners.borrow().iter() {
            listener.borrow_mut().mark_dirty();
        }
    }

    fn mark_dirty(&mut self) {
        self.clean = false;
    }
}

// fn main() {
//     let a = wrap(Signal::new(VariableType::new(Type::Any)));
//     let b = wrap(Signal::new(VariableType::new(Type::Any)));
//     let c = wrap(Signal::new(VariableType::new(Type::Any)));
//     let d = wrap(Signal::new(VariableType::new(Type::Any)));

//     let y = &a;
//     let x = y.borrow_mut();

//     a.borrow_mut().add_child(Rc::clone(&b));
//     a.borrow_mut().add_child(Rc::clone(&c));
//     b.borrow_mut().add_child(Rc::clone(&d));
//     a.borrow_mut().depth_first();
// }

fn main() {
    println!("Hello, world!");

    // let x;
    // if (Math.random() < 0.5) {
    //     x = "string";
    // } else {
    //     x = 1;
    // }
    //
    // console.log(x);
    //          ^? string | number

    let x = wrap(Signal::new(VariableType::new(Type::Any)));

    let x_string = wrap(Signal::new(VariableType::new(Type::String)));
    let x_number = wrap(Signal::new(VariableType::new(Type::Number)));

    let x_derived = Signal::from_signals(&wrap(vec![x_string, x_number]), UpdateRule::Union);

    // println!("{:#?}", &x_derived);
    println!("{:#?}", &x_derived.borrow_mut().get());
}
