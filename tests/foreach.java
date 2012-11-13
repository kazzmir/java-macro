import java.io.File;

public class Token{
    public void foo(){
        macro foreach(){ (t:type i:id e:expression) body }{
          syntax(Iterator i = e.iterator();
                 while (i.hasNext()){
                   t element = (t) i.next();
                   body
                 })
        }

        foreach (File f files.get()){
            println(f);
        }
    }
}
