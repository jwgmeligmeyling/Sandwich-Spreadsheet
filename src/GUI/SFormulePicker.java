package GUI;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import Parser.Function;

@SuppressWarnings("serial")
public class SFormulePicker extends JDialog implements ActionListener, ListSelectionListener {
	
	// Implements action listener voor de clicks in je list
	@SuppressWarnings("rawtypes")
	private final JList lijst;
	private final Window window;
	private JLabel description;
	private JLabel arguments;
	
	private final static int DEFAULT_WINDOW_WIDTH = 450;
	private final static int DEFAULT_WINDOW_HEIGHT = 350;

	/**
	 * Constructor for the SFormulePicker
	 * @param window
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public SFormulePicker(Window window) {
		super(window, "Formules", true);
		this.window = window;
		setSize(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);
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
		
		Container container = getContentPane();
		container.setLayout(new BorderLayout());

		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		JButton okButton = new JButton("Ok");
		okButton.addActionListener(this);
		buttonPanel.add(okButton);
		
		setLayout(new BorderLayout());
		JPanel top= new JPanel();
		JPanel bot= new JPanel();
		description=new JLabel("description:");
		arguments=new JLabel("arguments:");
		bot.setLayout(new BorderLayout());
		bot.add(buttonPanel, BorderLayout.PAGE_END);
		bot.add(description, BorderLayout.PAGE_START);
		bot.add(arguments,BorderLayout.CENTER);
		JLabel tekst=new JLabel("Kies je formule.");
		top.add(tekst);
		container.add(top,BorderLayout.PAGE_START);
		container.add(listScroller, BorderLayout.CENTER);
		container.add(bot, BorderLayout.PAGE_END);
		setVisible(true);
	}
	
	@Override
	public void valueChanged(ListSelectionEvent arg0) {
		Function selected=(Function) lijst.getSelectedValue();
		description.setText("<html> description:"+selected.getDescription()+"</html>");
		arguments.setText("<html> arguments>" +selected.getArgumentList()+"</html>");
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
			
			if ( table.isEditing() && veld != null ) {
				String input = veld.getText();
				String head = input.substring(0, veld.getSelectionStart());
				head = (( input.length() == 0 ) ? "=" + head : head ) + functie.toString() + "(";
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
	public Function[] getSortedFunctions(){
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
