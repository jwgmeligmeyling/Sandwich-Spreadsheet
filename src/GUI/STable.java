package GUI;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.util.Arrays;

import javax.swing.DefaultCellEditor;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import File.Cell;
import File.Sheet;
import File.Sheet.Range;

public class STable extends JTable {
	private final Sheet sheet;
	private final JTextField formule;	
	

	public STable(final Sheet sheet, JTextField formule) {
		super(new AbstractTableModel() {
			public String getColumnName(int column) {
				if ( column == 0 ) {
					return "";
				}
				return sheet.getColumnLetter(column - 1);
			}

			public int getRowCount() {
				return sheet.getRowCount();
			}

			public int getColumnCount() {
				return sheet.getColumnCount() + 1;
			}

			public Object getValueAt(int row, int col) {
				if ( col == 0 ) {
					return row + 1;
				}
				return sheet.getCellAt(col - 1, row).getValue();
			}

			public boolean isCellEditable(int row, int column) {
				return column != 0;
			}

			public void setValueAt(Object value, int row, int col) {
				sheet.getCellAt(col - 1, row).setInput(value.toString());
				fireTableCellUpdated(row, col);
			}
		}, null, null);
		
		SelectionHandler handler = new SelectionHandler();
		getSelectionModel().addListSelectionListener(handler);
		getColumnModel().getSelectionModel().addListSelectionListener(handler);
		
		setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		setRowSelectionAllowed(true);
	    setColumnSelectionAllowed(true);
	    setCellSelectionEnabled(true);
	    setGridColor(new Color(220, 220, 255));

		
		getColumnModel().getColumn(0).setPreferredWidth(15);
		getColumnModel().getColumn(0).setCellRenderer(new TableCellRenderer() {
			
            @Override
            public Component getTableCellRendererComponent(JTable x, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                boolean selected = getSelectionModel().isSelectedIndex(row);
                Component component = x.getTableHeader().getDefaultRenderer().getTableCellRendererComponent(x, value, false, false, -1, -2);
                ((JLabel) component).setHorizontalAlignment(JLabel.CENTER);
                if (selected) {
                    component.setFont(component.getFont().deriveFont(Font.BOLD));
                } else {
                    component.setFont(component.getFont().deriveFont(Font.PLAIN));
                }
                return component;
            }
        });
		
		this.sheet = sheet;
		this.formule = formule;
	}
	
	@Override
	public TableCellEditor getCellEditor(int row, int column) {
		return new CustomTableCellEditor();
	}
	
	public class CustomTableCellEditor extends DefaultCellEditor implements TableCellEditor {
		String value;
		
		public CustomTableCellEditor() {
			super(new JTextField());
			getComponent().setName("Table.editor");
		}
		
		@Override
		public boolean stopCellEditing() {
			formule.setEnabled(false);
			formule.setText("");
			value = (String) super.getCellEditorValue();
			return super.stopCellEditing();
		}
		
		@Override
		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
			Cell cell = sheet.getCellAt(column - 1, row);
			Object input = cell.getInput();
			this.value = null;
			((JComponent) getComponent()).setBorder(new LineBorder(Color.black));
			formule.setText(input.toString());
			formule.setEnabled(true);
			return super.getTableCellEditorComponent(table, input, isSelected, row, column);
		}
		
		@Override
		public Object getCellEditorValue() {
			return value;
		}
	}
	
	private Range previousRange;
	
	class SelectionHandler implements ListSelectionListener {
		
		@Override
		public void valueChanged(ListSelectionEvent e) {
			if ( e.getValueIsAdjusting() )
				return;
			int[] selectedColumns = getSelectedColumns();
			int[] selectedRows = getSelectedRows();
			
			if ( selectedColumns != null & selectedRows != null ) {
				int selectedColumnsCount = selectedColumns.length;
				int selectedRowsCount = selectedRows.length;
				
				if ( selectedColumnsCount == 0 || selectedRowsCount == 0 ) {
					return;
				}
				
				int colLeft = selectedColumns[0] - 1;
				int rowUp = selectedRows[0];
				int colRight = selectedColumns[selectedColumnsCount-1] - 1;
				int rowDown = selectedRows[selectedRowsCount -1];
					
				if ( colLeft == 0) {
					return; // First column is reserved for row index
				}
				
				Range range = sheet.getRange(colLeft, rowUp, colRight, rowDown);
				if (!range.equals(previousRange)) {
					previousRange = range;
					System.out.println("Selected range: "  + range.toString());
					/*
					 * TODO Liam formule.setText() -> cell.getInput() wanneer
					 * range.isSingleCell(). Daarna kijken hoe we aanpassingen
					 * in het formule tekstveld kunnen doorvoeren naar de cell.
					 * ( range.getCellArray()[0] -> Cell.setInput(String) )
					 * 
					 * TODO Functie-namen en celverwijzingen na invoeren omzetten in hoofdletters.
					 * TODO Altijd de formule (Cell.input) van de ACTIEVE cel zichtbaar in de formulebalk.
					 * TODO Verwijzingen automatisch invoeren als in edit-mode andere cellen worden geselecteerd.
					 * TODO Check voordat de input wordt ingesteld op net ingevoerde waarde of er 
					 * kringverwijzingen in voorkomen (let op! kunnen direct en indirect zijn!!!)
					 */
				}
			}
		}
	}
}
