package Tests;

import java.util.ArrayList;

import org.junit.Test;

import File.Sheet;
import File.Sheet.Position;

public class positionTest {
	

	private ArrayList<Integer> lijst = new ArrayList<Integer>();
	
	
	@Test
	public void test() {
		Sheet sheet = new Sheet();
		
		int count = 300;
		
		for ( int x = 0; x < count; x++){
			for( int y=0; y<count; y++){
				Position p=sheet.new Position(x,y);
				Integer hash = p.hashCode();
				if ( lijst.contains(hash)) {
					throw new RuntimeException("Failed for " + p.toString());
				} else {
					lijst.add(hash);
				}
			}
		}
		
		}
		


}
