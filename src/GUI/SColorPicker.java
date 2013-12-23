package GUI;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JColorChooser;
import javax.swing.JDialog;
import javax.swing.JPanel;

@SuppressWarnings("serial")
public class SColorPicker extends JDialog implements ActionListener {
	
	public SColorPicker(Frame owner, String title, boolean modal) {
		super(owner, title, modal);
		
		JColorChooser colorPicker = new JColorChooser();
		colorPicker.setPreviewPanel(new JPanel());
		this.add(colorPicker);
	}
	
	
	
	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub
		
	}
}