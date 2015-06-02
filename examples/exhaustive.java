class A extends Object {
    A() {
        super();
    }
}

class B extends Object {
    B() {
        super();
    }
}

class C extends Object {
    A a;

    C(A a) {
        super();
        this.a = a;
    }

    B hehe (B b) {
        return new B();
    }
}

class Pair extends Object {
    Object fst;
    Object snd;

    Pair(Object fst, Object snd) {
        super();
        this.fst = fst;
        this.snd = snd;
    }

    Pair setfst(Object newfst) {
        return new Pair(newfst, this.snd);
    }
}

class Triple extends Pair {
    Object third;

    Triple(Object fst, Object snd, Object third) {
        super(fst, snd);
        this.third = third;
    }

    Triple setThird(Object newThird) {
        return new Triple(this.fst, this.snd, newThird);
    }

    Pair setfst(Object newfst) {
        return new Triple(newfst, this.snd, this.third);
    }
}

class Tester extends Object {
    Tester() {
        super();
    }

    Pair acceptPair(Pair p) {
        return p;
    }

    Triple acceptTriple(Triple t) {
        return t;
    }
}

new Tester().acceptTriple((Triple) new Triple(new A(), new B(), new C(new A())).setfst(new B()))
