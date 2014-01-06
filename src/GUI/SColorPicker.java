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
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

@SuppressWarnings("serial")
public class SColorPicker extends JFrame implements ActionListener {
	
	JColorChooser colorPicker;
	JButton jbnOk;
	JButton jbnCancel;
	JPanel jpExamplePanel;
	JLabel jlbExampleBackground;
	JLabel jlbExampleText;
	
	public SColorPicker(String title, Color oldColor) {
		//super(owner, title, modal);
		super(title);
		
		jpExamplePanel = new JPanel();
		jpExamplePanel.setLayout(new FlowLayout());
		
		jlbExampleText = new JLabel("Example text");
		jlbExampleBackground = new JLabel("Example background");
		jlbExampleBackground.setSize(50, 15);
		
		jpExamplePanel.add(jlbExampleText);
		jpExamplePanel.add(jlbExampleBackground);
		
		colorPicker = new JColorChooser(oldColor);
		colorPicker.setPreviewPanel(jpExamplePanel);
		
		jbnOk = new JButton("OK");
		jbnOk.addActionListener(this);
		jbnCancel = new JButton("Cancel");
		jbnCancel.addActionListener(this);
		
		//jlbExampleLabel = new JLabel("Example:");
		
		setSize(600, 350);
		setResizable(false);
		setLocationRelativeTo(null);
		setAlwaysOnTop(false);
		
		Container container = getContentPane();
		container.setLayout(new BorderLayout());
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		//buttonPanel.add(jlbExampleLabel);
		//buttonPanel.add(jlbExample);
		buttonPanel.add(jbnCancel);
		buttonPanel.add(jbnOk);
		
		setLayout(new BorderLayout());
		container.add(colorPicker, BorderLayout.CENTER);
		container.add(buttonPanel, BorderLayout.PAGE_END);
		
		setVisible(true);
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		
		// TODO Er is blijkbaar geen ActionEvent door de colorPicker, dit is een probleem omdat er nu geen example wordt getoond.
		//if (e.getSource() == colorPicker) {
			jlbExampleBackground.setBackground(colorPicker.getColor());
			jlbExampleText.setForeground(colorPicker.getColor());
			System.out.println(colorPicker.getColor().toString());
		//}
		
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