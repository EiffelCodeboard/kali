import p1.*; import p2.*; import org.junit.Test;
import static org.junit.Assert.*;
public class TestB {
@Test
public void testPlus() {
 B b1= new B();
 C c1= new C();
 D d1= new D();
 E e1= new E();
 int result =b1.one()+c1.two()+d1.three()+e1.four();;
 
assertEquals(10, result); } }