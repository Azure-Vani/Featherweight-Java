class A extends Object {
    int a;

    A(int b) {
        super();
        this.a = b;
    }

    C f(B oth) {
        return new C();
    }
}

class B extends Object {
    int c;

    B() {
        super();
        this.c = 3;
    }
    
    C f() {
        return new C();
    }
}

class C extends Object {
    C() {
        super();
    }
}

class D extends Object {
    D() {
        super();
    }
    B g() {
        return new B();
    }
    int h() {
        return new B();
    }
}

(new A(1)).f((new D()).g())
