import static org.junit.jupiter.api.Assertions.*;

import java.awt.geom.Point2D;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

class STONETest {
	@Test
	void Can_compute_signed_area() {
		var t1 = new Triangle(new Point2D.Double(0,5), new Point2D.Double(3,0));
		assertEquals(0.5*5*3, Math.abs(t1.signedArea()));
		var t2 = new Triangle(new Point2D.Double(3,0), new Point2D.Double(0,5));
		assertEquals(-t1.signedArea(), t2.signedArea());
	}
	
	@Test
	void Can_make_triangles_from_points() {
		var ts = Triangle.fromPoints(new Point2D.Double[] {
				new Point2D.Double(-5,-5),
				new Point2D.Double(-5, 5),
				new Point2D.Double( 5, 5),
				new Point2D.Double( 5,-5),
		});
		var area = Arrays.stream(ts).mapToDouble(Triangle::signedArea).sum();
	    assertEquals(100.0, Math.abs(area));
	}
	
	@Test
	void moveOrigin_moves_points_to_honor_origin() {
		var points = new Point2D.Double[] {
			new Point2D.Double(10,5),
			new Point2D.Double(4, 6),
			new Point2D.Double(-2, -2),
		};
		var origin = new Point2D.Double(5,5);
		var adjusted = STONE.moveOrigin(origin, points);
		assertEquals(new Point2D.Double(5,0), adjusted[0]);
		assertEquals(new Point2D.Double(-1,1), adjusted[1]);
		assertEquals(new Point2D.Double(-7,-7), adjusted[2]);
	}
	
	@Test
	void findOrigins_finds_origins_that_place_figure_in_separate_quadrants() {
		var points = new Point2D.Double[] {
				new Point2D.Double(10,5),
				new Point2D.Double(4, 6),
				new Point2D.Double(-2, -1),
			};
		var origins = STONE.findOrigins(points);
		var o1points = STONE.moveOrigin(origins[0], points);
	    Arrays.stream(o1points).forEach(p -> assertTrue(p.getX() > 1.0));
	    Arrays.stream(o1points).forEach(p -> assertTrue(p.getY() > 1.0));
		
		var o2points = STONE.moveOrigin(origins[1], points);
		Arrays.stream(o2points).forEach(p -> assertTrue(p.getX() < -1.0));
		Arrays.stream(o2points).forEach(p -> assertTrue(p.getY() > 1.0));
	}
	
	@Test
	void finds_intersection_of_lines() {
		var p1 = new Point2D.Double(0, 0);
		var p2 = new Point2D.Double(10, 0);
		var result = STONE.findIntersection(p1, 0.5, p2, -0.5);
		assertEquals(new Point2D.Double(5, 2.5), result);
	}
	
	@Test
	void can_bisect_triangle_area_with_slope() {
		var t = new Triangle(new Point2D.Double(10, 20), new Point2D.Double(20, 10));
		//assertEquals(9, t.signedArea());
		var result = t.bisect(1/2);
		assertEquals(result[0].signedArea(), result[1].signedArea());
		assertEquals(t.signedArea(), Arrays.stream(result).mapToDouble(Triangle::signedArea).sum());
	}
	
	/*
	@Test
	void finds_slope_bisecting_origin() {
		var triangles = Triangle.fromPoints(new Point2D.Double[] {
				new Point2D.Double(10, 10),
				new Point2D.Double(10, 50),
				new Point2D.Double(50, 50),
				new Point2D.Double(50, 10),
		});
		var slope = STONE.areaBisectingSlopeThroughOrigin(triangles);
		assertEquals(0.5, slope);
	}*/
	
}