package org.foo;

public class Top {
    public static class CInner1<T, S> {
        public void foo_bar() {
            if (true) {
                class CInner1_CLocal1 implements Serializable {
                    void foo_bar() {
                        System.out.println("");
                        if (true) {
                            class CInner1_CLocal1_CLocal1 extends Object {
                                public void foo_bar() {
                                    System.out.println("");
                                }
                            }
                        }
                    }
                }
            }

            class CInner1_CLocal2 extends Object {
                void foo_bar() {
                    System.out.println("");
                }
            }
        }

        private Object obj = new Object();
        class CInner1_CInner1 {
        }
    }

    interface IInner1 {
        interface IInner1_IInner1 extends A<B, C>, Serializable {
            void foo_bar();
        }

        static class IInner1_CInner1 {
            public void foo_bar() {
                if (true) {
                    System.out.println("");
                }
            }
        }
    }

    enum EInner1 {
        A("a"),
        B("b");
        private EInner1() {
        }
        public void foo_bar() {
            System.out.println("");
        }

        enum EInner1_EInner1 {
            C, D
        }
    }
}
