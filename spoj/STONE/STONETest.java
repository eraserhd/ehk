import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class STONETest {

	@Test
	void Triangle_area() {
		assertEquals(3.0*5.0/2.0, new Triangle(new Point(3,0), new Point(0,5)).area(), 1e-5);
	}
	
	@Test
	void Triangle_leftAndRightArea_positive_x() {
		var t = new Triangle(new Point(5,1), new Point(1, 5));
		var lra = t.leftAndRightArea(Math.PI/4.0);
		assertEquals(lra[0], lra[1], 1e-5);
	}
	
	@Test
	void Triangle_leftAndRightArea_negative_x() {
		var t = new Triangle(new Point(-5,1), new Point(-1, 5));
		var lra = t.leftAndRightArea(Math.PI*3/4.0);
		assertEquals(lra[0], lra[1], 1e-5);
	}
	
	@Test
	void Test_case_1() {
		var points = new Point[] {
				new Point(5, 0),
				new Point(0, 5),
				new Point(-5, 0),
				new Point(0, -5),
		};
		final Point result = STONE.solve(points);
		assertEquals(new Point(6, 0), result);
	}

}
