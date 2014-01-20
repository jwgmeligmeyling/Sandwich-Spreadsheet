package GUI;

import java.awt.Color;

import javax.swing.JLabel;
import javax.swing.border.BevelBorder;

@SuppressWarnings("serial")
public class SStatusBar extends JLabel {
	
	public SStatusBar() {
		super("Sandwich Spreadsheet");
		this.setBorder(new BevelBorder(BevelBorder.LOWERED, Color.WHITE, Color.GRAY));
		this.setBackground(new Color(100, 100, 200));
		this.setVisible(true);
	}
	
}
