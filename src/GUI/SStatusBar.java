package GUI;

import java.awt.Color;
import javax.swing.JLabel;

@SuppressWarnings("serial")
public class SStatusBar extends JLabel {
	
	private int statusbarHeight = 25;
	
	private final Window window;
	
	public SStatusBar(Window windowIn) {
		super("Dit is de status bar");
		this.window = windowIn;
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
