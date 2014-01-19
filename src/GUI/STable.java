package GUI;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;

import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.DefaultCellEditor;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import File.Cell;
import File.Sheet;
import File.Sheet.Range;

@SuppressWarnings("serial")
/**
 * The STable extends the JTable with row numbers, range selection and uses
 * a <code>Sheet</code> instance as data model.
 * @author Jan-Willem Gmelig Meyling
 *
 */
public class STable extends JTable {

	/**
	 * A reference to this <code>STable</code> instance for use in inner classes
	 */
	private final STable table = this;
	/**
	 * A reference to the <code>FormuleBalk</code> instance in the GUI.
	 */
	private final FormuleBalk formuleBalk;
	/**
	 * A reference to the <code>Sheet</code> instance used as data model for this
	 * table instance.
	 */
	private final Sheet sheet;
	
	private boolean close = false;
	
	private Range selectedRange;
	private JTextField currentEditor;

	private static final Color DEFAULT_GRID_COLOR = new Color(206, 206, 206);
	private static final Color DEFAULT_SELECTION_COLOR = new Color(190, 220, 255);
	private static final Color DEFAULT_SELECTION_TEXT = new Color(0, 0, 0);
	
	private static final int DEFAULT_COLUMN_WIDTH = 50;

	private final static Long EDITOR_SHIFT_UP = 234234234234234234l;
	private final static Long EDITOR_SHIFT_RIGHT = 1234234234123123l;
	private final static Long EDITOR_SHIFT_DOWN = 2341232423175654l;
	private final static Long EDITOR_SHIFT_LEFT = 9879870345322125l;
	private final static Long EDITOR_UP = 234234234123234234l;
	private final static Long EDITOR_RIGHT = 1234231214123123l;
	private final static Long EDITOR_DOWN = 234125623175654l;
	private final static Long EDITOR_LEFT = 987987036722125l;

	/**
	 * Constructor for the table
	 * @param sheet
	 * @param formule
	 */
	public STable(Sheet sheet, FormuleBalk formule) {
		/*
		 * Call the super constructor with our custom TableModel
		 * that uses a Sheet instance for the data
		 */
		super(new TableModel(sheet), null, null);
		/*
		 * Turn off the JTable.autoStartsEdit, so that we
		 * can hook in to this manually at the processKeyBinding
		 * function. 
		 */
		this.putClientProperty("JTable.autoStartsEdit", false);
		/*
		 * Set some final attributes, and create the cross reference
		 * with the Sheet
		 */
		this.sheet = sheet;
		this.sheet.setSTable(this);
		this.formuleBalk = formule;
		/*
		 * Set the default renderer for the STable
		 */
		this.setDefaultRenderer(Cell.class, new CustomCellRenderer());
		/*
		 * Create a custom mouse adapter to hook into the selections
		 */
		CustomMouseAdapter adapter = new CustomMouseAdapter();
		addMouseListener(adapter);
		addMouseMotionListener(adapter);
		addListSelectionListener(new SelectionHandler());
		/*
		 * Limit the Cell selection to a single interval, and allow
		 * selection of single cells, columns and rows.
		 */
		setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		setCellSelectionEnabled(true);
		/*
		 * Set the color defaults
		 */
		gridColor = DEFAULT_GRID_COLOR;
		selectionBackground = DEFAULT_SELECTION_COLOR;
		selectionForeground = DEFAULT_SELECTION_TEXT;
		autoResizeMode = AUTO_RESIZE_OFF;
		/*
		 * Create the renderers for the column and row labels
		 */
		tableHeader.setDefaultRenderer(new HeaderNameRenderer(tableHeader .getDefaultRenderer()));
		columnModel.getColumn(0).setPreferredWidth(DEFAULT_COLUMN_WIDTH);
		columnModel.getColumn(0).setCellRenderer(new RowNumberRenderer());
	}
	
	/**
	 * Method to get the selected cells as <code>Range</code> object.
	 * 
	 * @return Range object containing the selected cells, or null if the
	 *         selection was empty or contained the row headers.
	 */
	public Range getSelectedRange() {
		int[] selectedColumns = getSelectedColumns();
		int[] selectedRows = getSelectedRows();
	
		if (selectedColumns.length == 0 || selectedRows.length == 0
				|| selectedColumns[0] == 0) {
			return null;
		}
	
		int colLeft = selectedColumns[0] - 1;
		int rowUp = selectedRows[0];
		int colRight = selectedColumns[selectedColumns.length - 1] - 1;
		int rowDown = selectedRows[selectedRows.length - 1];
		
		return sheet.getRange(colLeft, rowUp, colRight, rowDown);
	}

	/**
	 * @return the {@code Sheet} for this {@code STable}
	 */
	public Sheet getSheet() {
		return sheet;
	}

	/**
	 * <p>Because the default key handler was a bit too limited to prevent
	 * a new Cell editor being opened on every key, we needed to prevent
	 * the default behavior of the key handler.</p>
	 * 
	 * <p>This key handler makes an exception for key events where there is
	 * no cell editor open and the key is a alphanumeric character. A new
	 * cell editor will be opened and the character will be appended to the
	 * input field.</p>
	 * 
	 * <p>For different key events, like the arrow keys, the key event 
	 * is delegated to the super implementation for this method.</p>
	 */
	@Override
	protected boolean processKeyBinding(KeyStroke ks, KeyEvent e,
           int condition, boolean pressed) {
		char keyChar = e.getKeyChar();
		if ( keyChar == '\n' && isEditing() ) {
			this.close = true;
			this.cellEditor.stopCellEditing();
		} else if (  keyChar != '\n' && keyChar != KeyEvent.CHAR_UNDEFINED &&
				this.hasFocus() && !isEditing() ) {
			int leadRow = getSelectionModel().getLeadSelectionIndex();
            int leadColumn = getColumnModel().getSelectionModel().
                               getLeadSelectionIndex();
            if (leadRow != -1 && leadColumn != -1 && !isEditing()) {
                if (!editCellAt(leadRow, leadColumn, e)) {
                    return false;
                } else if ( currentEditor != null ) {
                	currentEditor.setText(currentEditor.getText() + keyChar);
                	currentEditor.requestFocus();
                }
            }
		} else {
			return super.processKeyBinding(ks, e, condition, pressed);
		}
		return true;
	}
	
	/**
	 * Listeners bound only to the selection model are not activated when
	 * the selection changes vertically, thus we bind the ListSelectionListener
	 * to the column model as well
	 */
	public void addListSelectionListener(ListSelectionListener listener) {
		selectionModel.addListSelectionListener(listener);
		columnModel.getSelectionModel().addListSelectionListener(listener);
	}

	/**
	 * The getCellEditor function is called to create a new TableCellEditor.
	 * In this function we prepare a new JTextField, with special events
	 * bound to it. This is used to make selections and the relationship
	 * with the formula bar in the GUI work with the current cell editor.
	 */
	@Override
	public TableCellEditor getCellEditor(int row, int column) {
		currentEditor = new JTextField();
		registerKeyStrokes();
		formuleBalk.setCurrentTable(table);
		formuleBalk.setCurrentEditor(currentEditor);
		return new CustomTableCellEditor();
	}
	
	/**
	 * Register the key strokes for creating ranges from the <code>CellEditor</code>
	 */
	private void registerKeyStrokes() {
		currentEditor.getInputMap().put(KeyStroke.getKeyStroke("shift UP"), EDITOR_SHIFT_UP);
		currentEditor.getInputMap().put(KeyStroke.getKeyStroke("shift RIGHT"), EDITOR_SHIFT_RIGHT);
		currentEditor.getInputMap().put(KeyStroke.getKeyStroke("shift DOWN"), EDITOR_SHIFT_DOWN);
		currentEditor.getInputMap().put(KeyStroke.getKeyStroke("shift LEFT"), EDITOR_SHIFT_LEFT);
		currentEditor.getInputMap().put(KeyStroke.getKeyStroke("UP"), EDITOR_UP);
		currentEditor.getInputMap().put(KeyStroke.getKeyStroke("RIGHT"), EDITOR_RIGHT);
		currentEditor.getInputMap().put(KeyStroke.getKeyStroke("DOWN"), EDITOR_DOWN);
		currentEditor.getInputMap().put(KeyStroke.getKeyStroke("LEFT"), EDITOR_LEFT);
		currentEditor.getActionMap().put(EDITOR_SHIFT_UP, new SelectAction(0, -1,   true, true));
		currentEditor.getActionMap().put(EDITOR_SHIFT_RIGHT, new SelectAction(1, 0, true, true));
		currentEditor.getActionMap().put(EDITOR_SHIFT_DOWN, new SelectAction(0, 1,  true, true));
		currentEditor.getActionMap().put(EDITOR_SHIFT_LEFT, new SelectAction(-1, 0, true, true));
		currentEditor.getActionMap().put(EDITOR_UP, new SelectAction(0, -1,   false, false));
		currentEditor.getActionMap().put(EDITOR_RIGHT, new SelectAction(1, 0, false, false));
		currentEditor.getActionMap().put(EDITOR_DOWN, new SelectAction(0, 1,  false, false));
		currentEditor.getActionMap().put(EDITOR_LEFT, new SelectAction(-1, 0, false, false));
	}
	
	/**
	 * Update the value of the current <code>CellEditor</code> to match the
	 * selected cells.
	 */
	private void updateCellEditor() {
		if ( this.currentEditor == null ) {
			return;
		}
		
		Range range = getSelectedRange();
		if ( range.equals(this.selectedRange) || range.getTopLeft().equals(sheet.new Position(this.editingColumn - 1, this.editingRow))) {
			return;
		} else {
			this.selectedRange = range;
		}
		
		this.currentEditor.setText(new SelectionUpdater().insertOrReplace(range.toString()));
	}
	
	/**
	 * The Selection Updater is used to replace the existing range with a new selection
	 * @author Jan-Willem Gmelig Meyling
	 *
	 */
	private class SelectionUpdater {
		
		private final String value;
		private final int currentIndex;
		private final int length;
		
		private int start;
		private int end;
		
		/**
		 * The Selection Updater is used to replace the existing range with a new selection
		 */
		private SelectionUpdater() {
			value = currentEditor.getText();
			currentIndex = currentEditor.getSelectionEnd();
			length = value.length();
			start = seekStart();
			end = seekEnd();
		}
		
		/**
		 * @return the most likely start index for the "selected" range
		 */
		private int seekStart() {
			for ( int i = currentIndex - 1; i >= 0; i-- ) {
				char c = value.charAt(i);
				switch(c) {
				case ',':
				case ';':
				case '=':
				case '(':
					return i + 1;
				}
			}
			return 0;
		}
		
		/**
		 * @return the most likely end index for the "selected" range
		 */
		private int seekEnd() {
			for ( int i = currentIndex; i < length; i++ ) {
				char c = value.charAt(i);
				switch(c) {
				case ',':
				case ';':
				case ')':
					return i;
				}
			}
			return length;
		}
		
		/**
		 * @return true if the substring between the estimated begin and
		 * end index represent a range or cell reference.
		 */
		private boolean match() {
			return value.substring(start, end).matches("^((\\d+:\\d+)|([a-zA-Z]+:[a-zA-Z]+)|([a-zA-Z]+\\d+(:[a-zA-Z]+\\d+)?))$");
		}
		
		/**
		 * Replace the existing range, or append the new range to the end
		 * @param string
		 * @return updated String
		 */
		public String insertOrReplace(String string) {
			if ( length == 0 || value.charAt(0) != '=') {
				return value;
			} else if ( match() ) {
				return value.substring(0, start) + string + value.substring(end, length);
			} else {
				return value + string;
			}
		}
		
	}

	/**
	 * Action for the key strokes in the <code>CellEditor</code>. This is used for
	 * selecting ranges while in editing mode.
	 * @author Jan-Willem Gmelig Meyling
	 */
	private class SelectAction extends AbstractAction {
		
		private final int x;
		private final int y;
		private final boolean toggle;
		private final boolean extend;

		public SelectAction(int xOffset, int yOffset, boolean toggle,
				boolean extend) {
			super();
			this.x = xOffset;
			this.y = yOffset;
			this.toggle = toggle;
			this.extend = extend;
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			if ( currentEditor != null ) {
				String text = currentEditor.getText();
				if ( text != null && text.length() > 1 && text.charAt(0) != '=' ) {
					table.getCellEditor().stopCellEditing();
				}
			}
			
			int row = (y < 0) ? selectionModel.getMinSelectionIndex() + y
					: selectionModel.getMaxSelectionIndex() + y;
			
			int col = (x < 0) ? columnModel.getSelectionModel()
					.getMinSelectionIndex() + x : columnModel.getSelectionModel()
					.getMaxSelectionIndex() + x;
			
			table.changeSelection(row, col, toggle, extend);
			updateCellEditor();
		}
		
	}

	/**
	 * The CustomMouseAdapter allows selection of cells while in Editing state
	 * @author Jan-Willem Gmelig Meyling
	 *
	 */
	private class CustomMouseAdapter extends MouseAdapter {

		@Override
		public void mousePressed(MouseEvent e) {
			if (! isEditing()) {
				return;
			}
			
			Point p = e.getPoint();
			int row = table.rowAtPoint(p);
			int column = table.columnAtPoint(p);

			table.changeSelection(row, column, false, false);
			updateCellEditor();
		}

		@Override
		public void mouseDragged(MouseEvent e) {
			if (! isEditing()) {
				return;
			}

			Point p = e.getPoint();
			int row = table.rowAtPoint(p);
			int column = table.columnAtPoint(p);

			table.changeSelection(row, column, false, true);
			updateCellEditor();
		}

	}
	
	/**
	 * The custom TableCellEditor binds the <code>Sheet</code> class to this
	 * current <code>STable</code> instance.
	 * 
	 * @author Jan-Willem Gmelig Meyling
	 * @author Liam Clark
	 * 
	 */
	private class CustomTableCellEditor extends DefaultCellEditor {

		/**
		 * Constructor for the CustomTableCellEditor. Creates a new table cell
		 * editor with the JTextField stored in the currentEditor variable.
		 */
		public CustomTableCellEditor() {
			super(currentEditor);
			getComponent().setName("Table.editor");
		}

		@Override
		public Component getTableCellEditorComponent(JTable table,
				Object value, boolean isSelected, int row, int column) {
			try {
				//throw new Exception();
			} catch ( Exception e ) {
				e.printStackTrace();
			}
			close = false;
			String input = sheet.getInputAt(column - 1, row);
			currentEditor.setBorder(new LineBorder(Color.black));
			return super.getTableCellEditorComponent(table, input, isSelected,
					row, column);
		}

		@Override
		public Object getCellEditorValue() {
			return currentEditor.getText();
		}
		
		private boolean maintainEditor() {
			return currentEditor != null && !close &&
					currentEditor.getText().length() > 0 &&
					currentEditor.getText().charAt(0) == '=';
		}
		
		@Override
		public boolean stopCellEditing() {
			return maintainEditor() ? false : super.stopCellEditing();
		}
	}

	/**
	 * The custom TableCellEditor binds the <code>Sheet</code> class to this
	 * current <code>STable</code> instance.
	 * 
	 * @author Jan-Willem Gmelig Meyling
	 * @author Liam Clark
	 * 
	 */
	private static class TableModel extends AbstractTableModel {

		private final Sheet sheet;
		
		private static final int DEFAULT_COLUMN_COUNT = 26;
		private static final int DEFAULT_ROW_COUNT = 200;

		/**
		 * Constructor for the TableModel, sets a sheet variable.
		 * 
		 * @param sheet
		 */
		private TableModel(Sheet sheet) {
			this.sheet = sheet;
			sheet.init();
		}
		@Override
		public Class<?> getColumnClass(int columnIndex) {
	        return Cell.class;
	    }
		
		@Override
		public String getColumnName(int column) {
			if (column == 0) {
				return "";
			}
			return Sheet.getColumnLetter(column - 1);
		}

		@Override
		public int getRowCount() {
			return DEFAULT_ROW_COUNT;
		}

		@Override
		public int getColumnCount() {
			return DEFAULT_COLUMN_COUNT + 1;
		}

		@Override
		public Object getValueAt(int row, int col) {
			if (col == 0) {
				return row + 1;
			}
			return sheet.getValueAt(col - 1, row);
		}

		@Override
		public boolean isCellEditable(int row, int column) {
			return column != 0;
		}

		@Override
		public void setValueAt(Object value, int row, int col) {
			sheet.setValueAt(value, col - 1, row);
			fireTableCellUpdated(row, col);
		}

	}
	
	/**
	 * A custom Cell Renderer for the table
	 * @author Jan-Willem Gmelig Meyling
	 * @author Liam Clark
	 *
	 */
	private class CustomCellRenderer extends DefaultTableCellRenderer implements TableCellRenderer {
		
		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			Cell cell = sheet.getCellAt(column - 1, row);
			Component component = super.getTableCellRendererComponent(table, value, false, false, row, column);
			setBackground(getBackground(cell, isSelected));
			if ( cell != null)
				alterFont(component, cell);
			return component;
		}
		
		/**
		 * @param cell
		 * @param selected
		 * @return the background color for this cell
		 */
		private Color getBackground(Cell cell, boolean selected) {
			Color A = cell != null ? cell.getbColor() : null;
			Color B = selected ? DEFAULT_SELECTION_COLOR : table.getBackground();
			return mengKleuren(A,B);
		}
		
		/**
		 * @param A
		 * @param B
		 * @return a combination of the two colors
		 */
		private Color mengKleuren(Color A, Color B) {
			return A == null ? B : (B == null ? A : new Color(
					(A.getRed() + B.getRed()) / 2,
					(A.getGreen() + B.getGreen()) / 2,
					(A.getBlue() + B.getBlue()) / 2));
		}
		
		@SuppressWarnings({ "rawtypes", "unchecked" })
		private void alterFont(Component component, Cell cell) {
			Font font = component.getFont();
			Map attributes = font.getAttributes();
			if ( cell.isBold() ) { 
				attributes.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD);
			}
			if(cell.isUnderlined()){
				attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_LOW_ONE_PIXEL);
			}
			
			if(cell.isItalic()){
				attributes.put(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
			}
			Color Fcolor=cell.getfColor();
			if(Fcolor!=null){
				attributes.put(TextAttribute.FOREGROUND, Fcolor);
			}

			component.setFont(font.deriveFont(attributes));
		}
	}

	/**
	 * Renderer for the rowNumbers
	 * 
	 * @author Jan-willem Gmelig Meyling
	 * @author Liam Clark
	 * 
	 */
	private class RowNumberRenderer implements TableCellRenderer {

		@Override
		public Component getTableCellRendererComponent(JTable x, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			boolean selected = getSelectionModel().isSelectedIndex(row);
			Component component = x .getTableHeader() .getDefaultRenderer() .getTableCellRendererComponent(x, value, false, false, -1, -2);
			((JLabel) component).setHorizontalAlignment(JLabel.CENTER);
			if (selected) {
				component.setFont(component.getFont().deriveFont(Font.BOLD));
				component.setBackground(new Color(255,192,111));
			} else {
				component.setFont(component.getFont().deriveFont(Font.PLAIN));
			}
			return component;
		}

	}

	/**
	 * Override the header name renderer
	 * 
	 */
	private class HeaderNameRenderer implements TableCellRenderer {

		private TableCellRenderer delegate;

		/**
		 * Constructor for the header name renderer
		 * 
		 * @param delegate
		 */
		public HeaderNameRenderer(TableCellRenderer delegate) {
			this.delegate = delegate;
		}

		@Override
		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			Component component = delegate.getTableCellRendererComponent(table, value, false, false, row, column);
			((JLabel) component).setHorizontalAlignment(JLabel.CENTER);
			if (isSelected(column)) {
				component.setFont(component.getFont().deriveFont(Font.BOLD));
				component.setBackground(new Color(255,192,111));
			}
			return component;
		}

		/**
		 * Determine if a cell in the current column is selected
		 * @param column
		 * @return true if column is in the selected columns
		 */
		private boolean isSelected(int column) {
			for (int i : getSelectedColumns()) {
				if (i == column) {
					return true;
				}
			}
			return false;
		}
	}	
	
	/**
	 * The selection handler listens for new selected Ranges
	 * @author Liam Clark
	 *
	 */
	private class SelectionHandler implements ListSelectionListener {

		@Override
		public void valueChanged(ListSelectionEvent e) {
			tableHeader.repaint();
			if (!isEditing() && getSelectedRowCount() == 1
					&& columnModel.getSelectedColumnCount() == 1) {
				String input = sheet.getInputAt(getSelectedColumn() - 1, getSelectedRow());
				formuleBalk.setText(input);
			}
		}

	}
	

}
