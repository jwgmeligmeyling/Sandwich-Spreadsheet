package GUI;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;
import javax.swing.colorchooser.AbstractColorChooserPanel;

import File.Cell;
import Interfaces.Range;

@SuppressWarnings("serial")
public class SColorPicker extends JFrame implements ActionListener {

	JColorChooser colorPicker;
	JButton jbnOk;
	JButton jbnCancel;
	JPanel jpExamplePanel;
	JLabel jlbExampleBackground;
	JLabel jlbExampleText;
	private Window window;
	private boolean type;

	public SColorPicker(String title, Color oldColor, Window window, boolean t) {
		// super(owner, title, modal);
		super(title);
		this.window = window;
		type = t;
		colorPicker = new JColorChooser(oldColor);
		AbstractColorChooserPanel[] panels = colorPicker.getChooserPanels();
		for (AbstractColorChooserPanel accp : panels) {
			if (accp.getDisplayName().equals("Swatches")) {
				AbstractColorChooserPanel[] panels2 = { panels[0] };
				colorPicker.setChooserPanels(panels2);
			}
		}

		colorPicker.setPreviewPanel(new JPanel());

		jbnOk = new JButton("OK");
		jbnOk.addActionListener(this);
		jbnCancel = new JButton("Cancel");
		jbnCancel.addActionListener(this);

		// jlbExampleLabel = new JLabel("Example:");

		setSize(450, 180);
		setResizable(false);
		setLocationRelativeTo(null);
		setAlwaysOnTop(true);

		Container container = getContentPane();
		container.setLayout(new BorderLayout());

		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		// buttonPanel.add(jlbExampleLabel);
		// buttonPanel.add(jlbExample);
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
			dispose();
		}

		if (e.getSource() == jbnOk) {
			Range range = window.getSelectedRange();
			Cell[] selectedCells = range == null ? new Cell[0] : range
					.getCellArray();
			if (type == true) {
				for (Cell cell : selectedCells) {
					cell.setbColor(colorPicker.getColor());
				}
			}
			if (type == false) {
				for (Cell cell : selectedCells) {
					cell.setfColor(colorPicker.getColor());
				}
			}
			dispose();
		}
	}
}