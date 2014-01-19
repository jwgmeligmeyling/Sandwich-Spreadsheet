package GUI;

import java.awt.Color;
import java.awt.Frame;
import javax.swing.JLabel;

@SuppressWarnings("serial")
public class SStatusBar extends JLabel {
	
	@SuppressWarnings("unused")
	private static Frame window;
	
	private int statusbarHeight = 25;
	
	public SStatusBar(Frame parent) {
		super(" ");
		window = parent;
		//this.setBorder(new Border());
		this.setBackground(new Color(100, 100, 200));
		this.setVisible(true);
	}
	
	public int getStatusbarHeight() {
		return statusbarHeight;
	}
	
	public void setToolbarHeight(int statusbarHeightIn) {
		this.statusbarHeight = statusbarHeightIn;
	}
}
