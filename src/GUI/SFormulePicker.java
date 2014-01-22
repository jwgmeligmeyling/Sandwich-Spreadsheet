package GUI;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import Parser.Function;

@SuppressWarnings("serial")
public class SFormulePicker extends JDialog implements ActionListener,
		ListSelectionListener {

	// Implements action listener voor de clicks in je list
	private final Window window;
	
	@SuppressWarnings("rawtypes")
	private final JList lijst;
	private JLabel description;
	private JLabel arguments;

	private final static int DEFAULT_WINDOW_WIDTH = 450;
	private final static int DEFAULT_WINDOW_HEIGHT = 350;

	/**
	 * Constructor for the SFormulePicker.
	 * @param windowIn is the parent window.
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public SFormulePicker(Window windowIn) {
		super(windowIn, "Insert Function", true);
		
		window = windowIn;
		setResizable(false);
		setLayout(new BorderLayout(10, 10));

		JLabel emptyLeft = new JLabel();
		JLabel emptyRight = new JLabel();
		JLabel emptyTop = new JLabel();
		JLabel emptyBottom = new JLabel();

		JPanel container = new JPanel(new BorderLayout());
			JPanel centerBox = new JPanel(new BorderLayout());
				JPanel center = new JPanel(new BorderLayout());
			JPanel bottom = new JPanel(new BorderLayout());
				JPanel info = new JPanel(new BorderLayout());
				JPanel buttonPanel = new JPanel(new FlowLayout());
		
		lijst = new JList(getSortedFunctions());
		lijst.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		lijst.setLayoutOrientation(JList.VERTICAL);
		JScrollPane listScroller = new JScrollPane(lijst);
		lijst.setVisibleRowCount(-1);
		lijst.addListSelectionListener(this);
		lijst.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent evt) {
				if (evt.getClickCount() == 2) {
					actionPerformed(null);
				}
			}
		});
		
		centerBox.add(center, BorderLayout.CENTER);
		centerBox.setBorder(BorderFactory.createTitledBorder("Choose a function to insert:"));
		center.setBorder(new EmptyBorder(10, 10, 10, 10) );
		center.add(listScroller, BorderLayout.CENTER);

		JButton okButton = new JButton("Insert");
		okButton.addActionListener(this);
		buttonPanel.add(okButton);

		description = new JLabel("Description: ");
		arguments = new JLabel("Expected arguments: ");
		info.add(description, BorderLayout.PAGE_START);
		info.add(arguments, BorderLayout.PAGE_END);

		bottom.setPreferredSize(new Dimension(300, 100));
		bottom.add(info, BorderLayout.PAGE_START);
		bottom.add(buttonPanel, BorderLayout.PAGE_END);

		container.add(centerBox, BorderLayout.CENTER);
		container.add(bottom, BorderLayout.PAGE_END);

		add(emptyLeft, BorderLayout.WEST);
		add(emptyTop, BorderLayout.PAGE_START);
		add(emptyRight, BorderLayout.EAST);
		add(emptyBottom, BorderLayout.PAGE_END);
		add(container, BorderLayout.CENTER);

		pack();
		setSize(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);
		setLocationRelativeTo(null);
		setVisible(true);
	}

	@Override
	public void valueChanged(ListSelectionEvent arg0) {
		Function selected = (Function) lijst.getSelectedValue();
		description.setText("<html>Description: " + selected.getDescription() + "</html>");
		arguments.setText("<html>Expected arguments: " + selected.getArgumentList() + "</html>");
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		Object object = lijst.getSelectedValue();
		if (object != null) {
			dispose();

			Function functie = (Function) object;
			STable table = window.getCurrentTable();
			table.editCellAt(table.getSelectedRow(), table.getSelectedColumn());
			JTextField veld = (JTextField) table.getEditorComponent();

			if (table.isEditing() && veld != null) {
				String input = veld.getText();
				String head = input.substring(0, veld.getSelectionStart());
				head = ((input.length() == 0) ? "=" + head : head) + functie.toString() + "(";
				String tail = ")" + input.substring(veld.getSelectionEnd(), input.length());
				veld.setText(head + tail);
				veld.setSelectionStart(head.length());
				veld.setSelectionEnd(head.length());
				veld.requestFocus();
			}
		}
	}

	/**
	 * @return the functions sorted
	 */
	public Function[] getSortedFunctions() {
		List<Function> functieSelect = Arrays.asList(Function.values());
		Collections.sort(functieSelect, new Comparator<Function>() {
			@Override
			public int compare(Function o1, Function o2) {
				return o1.toString().compareTo(o2.toString());
			}
		});
		return functieSelect.toArray(new Function[functieSelect.size()]);
	}
}