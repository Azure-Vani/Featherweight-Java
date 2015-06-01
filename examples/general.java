class A extends Object {
    int a;

    A(int b) {
        super();
        this.a = b;
    }

    C p(B oth) {
        return new C(oth);
    }

    int fuck() {
        return 0;
    }
}

class B extends Object {
    int c;

    B() {
        super();
        this.c = 3;
    }
    
    C g(B oth) {
        return new C(oth);
    }
}

class C extends Object {
    int f;

    C(B oth) {
        super();
        this.f = oth.c;
    }
}

class D extends Object {
    D() {
        super();
    }
    B g() {
        return new B();
    }
    B h() {
        return new B();
    }
}

(new A(1)).p((new D()).g())
