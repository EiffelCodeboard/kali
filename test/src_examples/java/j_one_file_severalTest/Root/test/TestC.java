import org.junit.Test;
import static org.junit.Assert.*;
public class TestC {
@Test
public void testPlus() {
 Application my = new Application();
String result = my.getC();
assertEquals("C", result); } }