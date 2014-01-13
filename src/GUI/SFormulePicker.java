package GUI;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;


public class SFormulePicker extends JDialog implements ActionListener {
		// Implements action listener voor de clicks in je list
		    private final JList lijst;
		    private final Window window;
		  

		    // constructor
		    	public SFormulePicker(Window window) {
		    	super(window,"Formules",true);
		        this.window = window;
		        setSize(300,200);
		        lijst=new JList(Parser.Function.values());
		        
		        lijst.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		        lijst.setLayoutOrientation(JList.HORIZONTAL_WRAP);
		        JScrollPane listScroller = new JScrollPane(lijst);
		        listScroller.setPreferredSize(new Dimension(300, 200));
		        lijst.setVisibleRowCount(-1);
		        Container container = getContentPane();
				container.setLayout(new BorderLayout());
				
				JPanel buttonPanel = new JPanel();
				buttonPanel.setLayout(new FlowLayout());
				setLayout(new BorderLayout());
		        JButton cancelButton = new JButton("Cancel");
		        cancelButton.addActionListener(this);
		        buttonPanel.add(cancelButton);
		        
		        setLayout(new BorderLayout());
				container.add(listScroller, BorderLayout.CENTER);
				container.add(buttonPanel, BorderLayout.PAGE_END);
		    }

		 
	
		    
		
		@Override
		public void actionPerformed(ActionEvent arg0) {
			// TODO Auto-generated method stub
			Object object=lijst.getSelectedValue();
			if(object !=null){
				Parser.Function functie=(Parser.Function)object;
				JTextField veld= window.getCurrentEditor();
				if(veld!=null){
				String currentInput=veld.getText();
				if(currentInput.length()==0 || currentInput.charAt(0)!= '='){
					currentInput="="+currentInput;
				}
				veld.setText(currentInput+functie.toString()+"()");
				}
				dispose();
			}
			

		
		}
}
