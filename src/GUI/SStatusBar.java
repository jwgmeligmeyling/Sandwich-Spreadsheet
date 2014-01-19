package GUI;

import java.awt.Color;

import javax.swing.JLabel;
import javax.swing.border.BevelBorder;

@SuppressWarnings("serial")
public class SStatusBar extends JLabel {
	
	public SStatusBar() {
		super(" ");
		this.setBorder(new BevelBorder(BevelBorder.LOWERED, Color.GRAY, Color.DARK_GRAY));
		this.setBackground(new Color(100, 100, 200));
		this.setVisible(true);
	}
	
}
