package GUI;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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

import File.Cell;
import Parser.Function;

@SuppressWarnings("serial")
public class SFormulePicker extends JDialog implements ActionListener, ListSelectionListener {
	
	// Implements action listener voor de clicks in je list
	@SuppressWarnings("rawtypes")
	private final JList lijst;
	private final Window window;
	private JLabel description;

	/**
	 * Constructor for the SFormulePicker
	 * @param window
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public SFormulePicker(Window window) {
		super(window, "Formules", true);
		this.window = window;
		setSize(450, 200);
		lijst = new JList(getSortedFunctions());

		lijst.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		lijst.setLayoutOrientation(JList.HORIZONTAL_WRAP);
		JScrollPane listScroller = new JScrollPane(lijst);
		listScroller.setPreferredSize(new Dimension(300, 300));
		lijst.setVisibleRowCount(-1);
		lijst.addListSelectionListener(this);
		
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
		description=new JLabel();
		bot.setLayout(new BorderLayout());
		bot.add(buttonPanel, BorderLayout.CENTER);
		bot.add(description, BorderLayout.PAGE_START);
		
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
		description.setText(selected.getDescription());
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		Object object = lijst.getSelectedValue();
		if (object != null) {
			Function functie = (Function) object;
			JTextField veld = window.getCurrentEditor();
			
			if (veld != null) {
				String currentInput = veld.getText();
				if (currentInput.length() == 0 || currentInput.charAt(0) != '=') {
					currentInput = "=" + currentInput;
				}
				veld.setText(currentInput + functie.toString() + "()");
			}
			
			Cell selected = window.getSelectedCell();
			
			
			if(selected!= null){
				String currentInput= selected.getInput();
				
				if (currentInput.length() == 0 || currentInput.charAt(0) != '=') {
					currentInput = "=" + currentInput;
				}
				
				selected.setInput(currentInput+functie.toString()+"()");
			}
			
			dispose();
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
