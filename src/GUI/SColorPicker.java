package GUI;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

@SuppressWarnings("serial")
public class SColorPicker extends JFrame implements ActionListener {
	
	JColorChooser colorPicker;
	JButton jbnOk;
	JButton jbnCancel;
	
	public SColorPicker(String title, Color oldColor) {
		//super(owner, title, modal);
		super(title);
		
		colorPicker = new JColorChooser(oldColor);
		colorPicker.setPreviewPanel(new JPanel());
		
		jbnOk = new JButton("OK");
		jbnOk.addActionListener(this);
		jbnCancel = new JButton("Cancel");
		jbnCancel.addActionListener(this);
		
		setSize(600, 300);
		setResizable(false);
		setLocationRelativeTo(null);
		setAlwaysOnTop(false);
		
		Container container = getContentPane();
		container.setLayout(new BorderLayout());
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		buttonPanel.add(jbnCancel);
		buttonPanel.add(jbnOk);
		
		setLayout(new BorderLayout());
		container.add(colorPicker, BorderLayout.CENTER);
		container.add(buttonPanel, BorderLayout.PAGE_END);
		
		setVisible(true);
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == jbnCancel) {
			JOptionPane.showMessageDialog(this, "Pressed Cancel");
			
			
			dispose();
		}
		
		if (e.getSource() == jbnOk) {
			JOptionPane.showMessageDialog(this, "Pressed OK. Selected color: " + colorPicker.getColor().toString());
			
			
			dispose();
		}
	}
}