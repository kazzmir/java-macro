import java.io.File;

public class Token{

    public class Lambda{
        public abstract Object invoke(Object arg);
    }

    public void foo(){
        macro lambda(){ (x:id) body }{
          syntax(new Lambda(){
              public Object invoke(Object x){
                  body
              }
          })
        }

        Lambda l = lambda(x){ println(x); }
    }
}
