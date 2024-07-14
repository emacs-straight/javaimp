package org.foo;

public class Top {
    public static class CInner1<T, S> {
        public void foo() {
            if (true) {
                class CInner1_CLocal1 implements Serializable {
                    void foo() {
                        System.out.println("");
                        if (true) {
                            class CInner1_CLocal1_CLocal1 extends Object {
                                public void foo() {
                                    System.out.println("");
                                }
                            }
                        }
                    }
                }
            }

            class CInner1_CLocal2 extends Object {
                void foo() {
                    System.out.println("");
                }
            }
        }

        private Object obj = new Object();
        private Object obj = new Object() {
                @Override
                public String toString() {
                    return "asdf";
                }
            };
        abstract class CInner1_CInner1 {
            public void foo() {
            }

            abstract void abstract_method();

            public void bar() {
                System.out.println("");
            }

            abstract void baz();
        }
    }

    // public class LineCommentedClass {
    // }

    /*
      public class BlockCommentedClass {
      }
    */

    /*
     * public class BlockCommentedClass2 {
     * }
     */

    /**
     * public class JavadocCommentedClass {
     * }
     */


    interface IInner1 {
        static void foo() {
            if (true) {
                System.out.println("");
            }
        }

        String abstract_method();

        static class IInner1_CInner1 {
            public void foo() {
                if (true) {
                    System.out.println("");
                }
            }
        }

        void baz();

        default void defaultMethod(String arg1) {
            System.out.println("");
        }

        interface IInner1_IInner1 extends A<B, C>, Serializable {
            void foo();

            default String defaultMethod(String arg2) {
                System.out.println("");
            }

            void baz();
        }
    }

    enum EnumInner1 {
        A("a"),
        B("b");

        private EnumInner1() {
        }

        public void foo() {
            System.out.println("");
        }

        enum EnumInner1_EInner1 {
            C, D
        }
    }
}

class ColocatedTop {
    void foo() {
    }

    void bar(String arg1, String arg2) {
    }
}
