package Zandbak;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.Border;

import java.sql.*;

public class TestGridTab extends JFrame implements ActionListener
{
     private JTextField txtUser, txtPort, txtHost, txtDataB;
     private JPasswordField pwdPass;
     private JLabel lblUser, lblPort, lblHost, lblDataB, lblPass;
     private JTextArea txaRes;
     private String stUser, stPort, stHost, stDataB, stPass;
     private char chPass;
     private Container container;
     GridBagLayout gbl ;
     GridBagConstraints gbc = new GridBagConstraints();

     //Set up the GUI
     public TestGridTab()
     {
          gbl = new GridBagLayout();
          gbc = new GridBagConstraints();

          container = getContentPane();
          JTabbedPane mainTab = new JTabbedPane();

          //Create the first tab for the connections
          JPanel connect = new JPanel();

          connect.setLayout(gbl);

          //Set up the user label
          lblUser = new JLabel("User Name:");
          gbc.gridx = 0;
          gbc.gridy = 0;
          connect.add(lblUser,gbc);

          //Set up the user textfield

          txtUser = new JTextField(10);
          gbc.gridx = 1;
          gbc.gridy = 0;
          connect.add(txtUser,gbc);

          //add password components
          gbc.gridx = 0;
          gbc.gridy = 1;
          lblPass = new JLabel("Password");
          connect.add(lblPass,gbc);

          gbc.gridx = 1;
          gbc.gridy = 1;
          pwdPass = new JPasswordField(10);
          connect.add(pwdPass,gbc);

          //add port details
          gbc.gridx = 0; 
          gbc.gridy = 2;
          lblPort = new JLabel();
          connect.add(lblPort,gbc);

          //This will have the details of the second tabs gui components
          JPanel querypane = new JPanel();
          
          //This will have the admin gui
          JPanel adminPane = new JPanel();
          
          mainTab.addTab("Connection Settings",null,connect,"Database conection setings");
          mainTab.addTab("Queries",null,querypane,"Database queries");
          mainTab.addTab("Admin Settings",null,adminPane,"Admin only");
          container.add(mainTab);
          setSize(970,600);
          setVisible(true);
     }//end of gui set up


     public void actionPerformed(ActionEvent e)
     {
     	
     }
     
     public static void main(String[] args)
     {
          TestGridTab bookApp = new TestGridTab();
          bookApp.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
     }//end main

}//end class acmBooks