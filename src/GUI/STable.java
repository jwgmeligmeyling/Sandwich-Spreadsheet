package GUI;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.EventObject;

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
public class STable extends JTable implements ActionListener {

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
	
	private Range selectedRange;
	private boolean selectingRange;
	private JTextField currentEditor;

	private static final Color DEFAULT_GRID_COLOR = new Color(206, 206, 206);
	private static final Color DEFAULT_SELECTION_COLOR = new Color(190, 220, 255);
	private static final Color DEFAULT_SELECTION_TEXT = new Color(0, 0, 0);

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
		super(new TableModel(sheet), null, null);

		this.sheet = sheet;
		this.formuleBalk = formule;
		this.setDefaultRenderer(Cell.class, new CustomCellRenderer());

		CustomMouseAdapter adapter = new CustomMouseAdapter();
		addMouseListener(adapter);
		addMouseMotionListener(adapter);
		addKeyListener(new CustomKeyListener());
		
		SelectionHandler SH = new SelectionHandler();
		selectionModel.addListSelectionListener(SH);
		columnModel.getSelectionModel().addListSelectionListener(SH);

		setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		setCellSelectionEnabled(true);
		gridColor = DEFAULT_GRID_COLOR;
		selectionBackground = DEFAULT_SELECTION_COLOR;
		selectionForeground = DEFAULT_SELECTION_TEXT;
		autoResizeMode = AUTO_RESIZE_OFF;

		tableHeader.setDefaultRenderer(new HeaderNameRenderer(tableHeader
				.getDefaultRenderer()));

		columnModel.getColumn(0).setPreferredWidth(50);
		columnModel.getColumn(0).setCellRenderer(new RowNumberRenderer());
	}

	@Override
	public boolean editCellAt(int row, int column, EventObject e) {
		if (!selectingRange) {
			return super.editCellAt(row, column, e);
		}
		return false;
	}

	@Override
	public TableCellEditor getCellEditor(int row, int column) {
		currentEditor = new JTextField();
		currentEditor.addActionListener(this);
		registerKeyStrokes();
		formuleBalk.setCurrentTable(table);
		formuleBalk.setCurrentEditor(currentEditor);
		return new CustomTableCellEditor();
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
		
		Range range = sheet.getRange(colLeft, rowUp, colRight, rowDown);

		return range;
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
	
	@Override
	public void actionPerformed(ActionEvent e) {
		selectingRange = false;
		editingStopped(null);
	}

	/**
	 * Action for the key strokes in the <code>CellEditor</code>
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
	 * Key listener that ensures that the new editor gets it's focus 
	 * @author Jan-Willem Gmelig Meyling
	 *
	 */
	private class CustomKeyListener implements KeyListener {

		@Override
		public void keyTyped(KeyEvent e) {
			if (isEditing()) {
				currentEditor.requestFocus();
			}
		}

		@Override
		public void keyPressed(KeyEvent e) {

		}

		@Override
		public void keyReleased(KeyEvent e) {

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
			if (!selectingRange) {
				if (isEditing()) {
					selectingRange = true;
				} else {
					return;
				}
			}

			Point p = e.getPoint();
			int row = table.rowAtPoint(p);
			int column = table.columnAtPoint(p);

			table.changeSelection(row, column, false, false);
			updateCellEditor();
		}

		@Override
		public void mouseDragged(MouseEvent e) {
			if (!selectingRange) {
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
	 * Update the value of the current <code>CellEditor</code> to match the
	 * selected cells.
	 */
	private void updateCellEditor() {
		CustomTableCellEditor cellEditor = (CustomTableCellEditor) getCellEditor();
		Range range = getSelectedRange();

		if (currentEditor != null && cellEditor != null && range != null) {
			if (!range.equals(selectedRange)
					&& !range.contains(cellEditor.cell)) {
				String oldValue = (String) cellEditor.getCellEditorValue();
				if (selectedRange != null) {
					String oldRange = selectedRange.toString();
					if (oldValue.endsWith(oldRange)) {
						oldValue = oldValue.substring(0, oldValue.length()
								- oldRange.length());
					}
				}
				currentEditor.setText(oldValue + range.toString());
				selectedRange = range;
			}
		}

	}

	/**
	 * The custom TableCellEditor binds the <code>Sheet</code> class to this
	 * current <code>STable</code> instance.
	 * 
	 * @author Jan-Willem Gmelig Meyling, Liam Clark
	 * 
	 */
	private class CustomTableCellEditor extends DefaultCellEditor {
		private Cell cell;

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
			cell = sheet.getCellAt(column - 1, row);
			Object input = cell.getInput();
			currentEditor.setBorder(new LineBorder(Color.black));
			return super.getTableCellEditorComponent(table, input, isSelected,
					row, column);
		}

		@Override
		public Object getCellEditorValue() {
			return currentEditor.getText();
		}
		
		@Override
		public boolean stopCellEditing() {
			if ( ! selectingRange ) {
				return super.stopCellEditing();
			}
			return false;
		}
	}

	/**
	 * The custom TableCellEditor binds the <code>Sheet</code> class to this
	 * current <code>STable</code> instance.
	 * 
	 * @author Jan-Willem Gmelig Meyling, Liam Clark
	 * 
	 */
	private static class TableModel extends AbstractTableModel {

		private final Sheet sheet;

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
			return sheet.getColumnLetter(column - 1);
		}

		@Override
		public int getRowCount() {
			return sheet.getRowCount();
		}

		@Override
		public int getColumnCount() {
			return sheet.getColumnCount() + 1;
		}

		@Override
		public Object getValueAt(int row, int col) {
			if (col == 0) {
				return row + 1;
			}
			return sheet.getCellAt(col - 1, row).getValue();
		}

		@Override
		public boolean isCellEditable(int row, int column) {
			return column != 0;
		}

		@Override
		public void setValueAt(Object value, int row, int col) {
			Cell cell = sheet.getCellAt(col - 1, row);
			cell.setInput(value.toString());
			cell.update(this);
			fireTableCellUpdated(row, col);
		}

	}
	
	private class CustomCellRenderer extends DefaultTableCellRenderer implements TableCellRenderer{
		@Override
		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus,
				int row, int column) {
			Cell cell = sheet.getCellAt(column-1, row);
			Component component = super.getTableCellRendererComponent(table, value, false, false, row, column);
			if(isSelected	){
				setBackground(DEFAULT_SELECTION_COLOR);
				}
			if(cell.getbColor()==null){
				setBackground(table.getBackground());
				}
			else{
				setBackground(cell.getbColor());
			}
			return component;
		}
	}

	/**
	 * Renderer for the rowNumbers
	 * 
	 * @author Jan-willem Gmelig Meyling, Liam Clark
	 * 
	 */
	private class RowNumberRenderer implements TableCellRenderer {

		@Override
		public Component getTableCellRendererComponent(JTable x, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			boolean selected = getSelectionModel().isSelectedIndex(row);
			Component component = x
					.getTableHeader()
					.getDefaultRenderer()
					.getTableCellRendererComponent(x, value, false, false, -1,
							-2);
			((JLabel) component).setHorizontalAlignment(JLabel.CENTER);
			if (selected) {
				component.setFont(component.getFont().deriveFont(Font.BOLD));
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
			Component component = delegate.getTableCellRendererComponent(table,
					value, false, false, row, column);
			((JLabel) component).setHorizontalAlignment(JLabel.CENTER);
			if (isSelected(column)) {
				component.setFont(component.getFont().deriveFont(Font.BOLD));
			}
			return component;
		}

		// TODO @JanWillem: klopt dat eerste zinnetje hier wel? Er wordt toch niet per cel gecheckt, maar per kolom?
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
				Cell selectedCell = sheet.getCellAt(
						columnModel.getSelectedColumns()[0] - 1,
						getSelectedRow());
				formuleBalk.setText(selectedCell.getInput());
			}
		}

	}
	

}