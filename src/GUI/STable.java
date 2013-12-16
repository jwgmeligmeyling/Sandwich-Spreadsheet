package GUI;

import java.awt.Color;
import java.awt.Component;

import javax.swing.AbstractCellEditor;
import javax.swing.DefaultCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.border.LineBorder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;

import sun.reflect.misc.ReflectUtil;
import sun.swing.SwingUtilities2;
import File.Cell;
import File.Sheet;

public class STable extends JTable {
	private final Sheet sheet;
	private final JTextField formule;

	public STable(final Sheet sheet, JTextField formule) {
		super(new AbstractTableModel() {
			private final String[] columnNames = { "A", "B", "C", "D", "E", "F" };

			public String getColumnName(int column) {
				return columnNames[column].toString();
			}

			public int getRowCount() {
				return sheet.getRowCount();
			}

			public int getColumnCount() {
				return sheet.getColumnCount();
			}

			public Object getValueAt(int row, int col) {
				return sheet.getCellAt(col, row).getValue();
			}

			public boolean isCellEditable(int row, int column) {
				return true;
			}

			public void setValueAt(Object value, int row, int col) {
				sheet.getCellAt(col, row).setInput(value.toString());
				fireTableCellUpdated(row, col);
			}
        }, null, null);
		
		this.sheet = sheet;
		this.formule = formule;
	}
	
	@Override
	public TableCellEditor getCellEditor(int row, int column) {
		return new CustomTableCellEditor();
	}

	public class CustomTableCellEditor extends DefaultCellEditor implements TableCellEditor {

        Class[] argTypes = new Class[]{String.class};
        java.lang.reflect.Constructor constructor;
        Object value;

        public CustomTableCellEditor() {
            super(new JTextField());
            getComponent().setName("Table.editor");
        }

        public boolean stopCellEditing() {
            String s = (String)super.getCellEditorValue();
            formule.setEnabled(false);
            formule.setText("");
            try {
                if ("".equals(s)) {
                    if (constructor.getDeclaringClass() == String.class) {
                        value = s;
                    }
                    return super.stopCellEditing();
                }

                SwingUtilities2.checkAccess(constructor.getModifiers());
                value = constructor.newInstance(new Object[]{s});
            }
            catch (Exception e) {
                ((JComponent)getComponent()).setBorder(new LineBorder(Color.red));
                return false;
            }
            return super.stopCellEditing();
        }

        public Component getTableCellEditorComponent(JTable table, Object value,
                                                 boolean isSelected,
                                                 int row, int column) {
        	Cell cell = sheet.getCellAt(column, row);
        	Object input = cell.getInput();
            this.value = null;
            ((JComponent)getComponent()).setBorder(new LineBorder(Color.black));
            try {
                Class<?> type = table.getColumnClass(column);
                if (type == Object.class) {
                    type = String.class;
                }
                ReflectUtil.checkPackageAccess(type);
                SwingUtilities2.checkAccess(type.getModifiers());
                constructor = type.getConstructor(argTypes);
            }
            catch (Exception e) {
                return null;
            }
            formule.setText(input.toString());
            formule.setEnabled(true);
            return super.getTableCellEditorComponent(table, input, isSelected, row, column);
        }

        public Object getCellEditorValue() {
            return value;
        }
    }
}
