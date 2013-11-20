import javax.swing.JFrame;		//imports JFrame library
import javax.swing.JTextField;		//imports JButton library

import java.awt.GridLayout;		//imports GridLayout library

//import javax.swing.J

@SuppressWarnings("unused")
public class GridView {

	JFrame frame;													// creates frame
	JTextField[][] grid;											// names the grid of buttons

	public GridView(int width, int length) {	
		frame = new JFrame();
		frame.setLayout(new GridLayout(width, length));				// set layout
		grid = new JTextField[width][length];						// allocate the size of grid
		for (int y = 0; y < length; y++) {
			for (int x = 0; x < width; x++) {
				grid[x][y] = new JTextField("(" + x + "," + y + ")");	// creates new button
			frame.add(grid[x][y]);									// adds button to grid
			}
		}
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();												// sets appropriate size for frame
		frame.setVisible(true);										// makes frame visible
	}

	public static void main(String[] args) {
		new GridView(4,6);
		new GridView(5, 10);										// makes new ButtonGrid with 2 parameters
	}
}