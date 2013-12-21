package GUI;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.EventObject;

import javax.swing.DefaultCellEditor;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.LineBorder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import File.Cell;
import File.Sheet;
import File.Sheet.Range;

@SuppressWarnings("serial")
public class STable extends JTable implements ActionListener {
	
	private final STable table = this;
	private final FormuleBalk formuleBalk;
	private final Sheet sheet;
	
	private Range selectedRange;
	private boolean selectingRange;
	private JTextField currentEditor;

	private static final Color DEFAULT_GRID_COLOR = new Color(206,206,206);
	private static final Color DEFAULT_SELECTION_COLOR = new Color(190, 220, 255);
	private static final Color DEFAULT_SELECTION_TEXT = new Color(0,0,0);
	
	public STable(Sheet sheet, FormuleBalk formule) {
		super(new TableModel(sheet), null, null);
		
		this.sheet = sheet;
		this.formuleBalk = formule;
		
		CustomMouseAdapter adapter = new CustomMouseAdapter();
		addMouseListener(adapter);
		addMouseMotionListener(adapter);

		setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		setCellSelectionEnabled(true);
		gridColor = DEFAULT_GRID_COLOR;
		selectionBackground = DEFAULT_SELECTION_COLOR;
		selectionForeground = DEFAULT_SELECTION_TEXT;
		autoResizeMode = AUTO_RESIZE_OFF;
		
		tableHeader.setDefaultRenderer(new HeaderNameRenderer(tableHeader.getDefaultRenderer()));
		
		getColumnModel().getColumn(0).setPreferredWidth(50);
		getColumnModel().getColumn(0).setCellRenderer(new RowNumberRenderer());
	}
	
	@Override
	public boolean editCellAt(int row, int column, EventObject e) {
		if (! selectingRange ) {
			return super.editCellAt(row, column, e);
		}
		return false;
	}

	@Override
	public TableCellEditor getCellEditor(int row, int column) {
		currentEditor = new JTextField();
		currentEditor.addActionListener(this);
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

		return sheet.getRange(colLeft, rowUp, colRight, rowDown);
	}
	
	private class CustomMouseAdapter extends MouseAdapter {
		
		@Override
		public void mousePressed(MouseEvent e) {
			tableHeader.repaint();
			
			if  (! selectingRange ) {
				if ( isEditing() ) {
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
        	tableHeader.repaint();
        	
        	if (! selectingRange ) {
				return;
			}
        	
        	Point p = e.getPoint();
			int row = table.rowAtPoint(p);
			int column = table.columnAtPoint(p);
			
			table.changeSelection(row, column, false, true);
			updateCellEditor();
        }
        
        private void updateCellEditor() {
        	CustomTableCellEditor cellEditor = (CustomTableCellEditor) getCellEditor();
        	Range range = getSelectedRange();
        	
			if  ( currentEditor != null && cellEditor != null && range != null ) { 
				if (! range.equals(selectedRange) && ! range.contains(cellEditor.cell) )  {
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
        
    };
	
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
		public boolean stopCellEditing() {
			return false;
		//	value = (String) super.getCellEditorValue();
		//	return super.stopCellEditing();
		}

		@Override
		public Component getTableCellEditorComponent(JTable table,
				Object value, boolean isSelected, int row, int column) {
			cell = sheet.getCellAt(column - 1, row);
			Object input = cell.getInput();
			((JComponent) getComponent())
					.setBorder(new LineBorder(Color.black));
			return super.getTableCellEditorComponent(table, input, isSelected,
					row, column);
		}

		@Override
		public Object getCellEditorValue() {
			return currentEditor.getText();
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
		 * @param sheet
		 */
		private TableModel(Sheet sheet) {
			this.sheet = sheet;
			sheet.init();
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

	/**
	 * Renderer for the rowNumbers
	 * @author Jan-willem Gmelig Meyling, Liam Clark
	 *
	 */
	private class RowNumberRenderer implements TableCellRenderer {
		
		@Override
		public Component getTableCellRendererComponent(JTable x,
				Object value, boolean isSelected, boolean hasFocus,
				int row, int column) {
			boolean selected = getSelectionModel().isSelectedIndex(row);
			Component component = x
					.getTableHeader()
					.getDefaultRenderer()
					.getTableCellRendererComponent(x, value, false, false, -1, -2);
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
		
		/**
		 * Determine if a cell in the current column is selected
		 * @param column
		 * @return
		 */
		private boolean isSelected(int column) {
			for ( int i : getSelectedColumns() ) {
				if ( i == column ) {
					return true;
				}
			}
			return false;
		}
		
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		selectingRange = false;
		editingStopped(null);
	}
}
