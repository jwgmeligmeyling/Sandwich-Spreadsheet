package GUI;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

@SuppressWarnings("serial")
/**
 * A class for the formula JTextField container
 * This class implements ActionListener because it listens to itself for changes
 * @author Jan-Willem Gmelig Meyling, Liam Clark
 *
 */
public class FormuleBalk extends JTextField implements ActionListener {
	
	private STable currentTable;
	private JTextField currentEditor;
	private boolean preventLoop = false;
	
	/**
	 * Constructor
	 */
	public FormuleBalk() {
		super();
		this.addActionListener(this);
		this.getDocument().addDocumentListener(keyUp);
	}
	
	/**
	 * Method to get the current table
	 * @return
	 */
	public STable getCurrentTable() {
		return currentTable;
	}

	/**
	 * Method to set the current table
	 * @param currentTable
	 */
	public void setCurrentTable(STable currentTable) {
		this.currentTable = currentTable;
	}

	/**
	 * Method to get the current editor
	 * @return
	 */
	public JTextField getCurrentEditor() {
		return currentEditor;
	}

	/**
	 * Method to set the current cell editor. Remove the ActionListener for the
	 * previous editor, so we do not accidentally update multiple cells in
	 * separate sheets. At last, set the current editor to the given editor, and
	 * start listening for updates.
	 * 
	 * @param cellEditor
	 */
	public void setCurrentEditor(JTextField currentEditor) {		
		if ( currentEditor != null ) {
			currentEditor.removeActionListener(this);
		}
		
		this.currentEditor = currentEditor;
	
		this.currentEditor.getDocument().addDocumentListener(new CurrentEditorListener());
		this.setText(currentEditor.getText());
	}
	
	/**
	 * Listen on changes in this textfield, and send them to the cell editor
	 */
	private DocumentListener keyUp = new DocumentListener() {
		@Override
		public void insertUpdate(DocumentEvent de) {
			updateCellEditor();
		}
			
		@Override
		public void changedUpdate(DocumentEvent de) {

		}

		@Override
		public void removeUpdate(DocumentEvent de) {
			updateCellEditor();
		}

		private void updateCellEditor() {
			if (!preventLoop && currentEditor != null ) {
				preventLoop = true;
				currentEditor.setText(getText());
				preventLoop = false;
			}
		}
	};
	
	/**
	 * The FormuleBalkListener ensures that updates in the cell editor are
	 * sent to the Formula JTextField.
	 * @author Jan-Willem Gmelig Meyling
	 *
	 */
	private class CurrentEditorListener implements DocumentListener {

		@Override
		public void insertUpdate(DocumentEvent e) {
			updateCellEditor();
		}

		@Override
		public void removeUpdate(DocumentEvent e) {
			updateCellEditor();
			
		}

		@Override
		public void changedUpdate(DocumentEvent e) {
			// TODO Auto-generated method stub
			
		}
		
		private void updateCellEditor() {
			if (!preventLoop && currentEditor != null ) {
				preventLoop = true;
				setText(currentEditor.getText());
				preventLoop = false;
			}
		}
		
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if ( currentTable != null ) {
			currentTable.setValueAt(getText(), currentTable.getSelectedRow(), currentTable.getSelectedColumn());
			currentTable.removeEditor();
			currentTable.requestFocus();
			currentEditor = null;
		}
	}

}