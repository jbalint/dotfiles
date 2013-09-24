
import java.io.*;

import java.util.*;
import java.util.List;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;

class ViewFile extends File
{
	public ViewFile(String file)
	{
		super(file);
	}

	public ViewFile(File parent, String child)
	{
		super(parent, child);
	}

	public String toString()
	{
		return getName();
	}
}

class FileTreeModel implements TreeModel
{
	protected File rootPath;
	protected List<TreeModelListener> listeners =
		new ArrayList<TreeModelListener>();

	public FileTreeModel(File rootPath)
	{
		this.rootPath = rootPath;
	}

	public void fireTreeStructureChanged(TreeModelEvent event)
	{
		for(TreeModelListener l : listeners)
		{
			l.treeStructureChanged(event);
		}
	}

	/**
	 * non-API
	 */
	public void refreshAll()
	{
		TreeModelEvent event =
			new TreeModelEvent(this, new Object[] { rootPath });

		fireTreeStructureChanged(event);
	}

	/**
	 * non-API
	 */
	public void setRoot(File newRoot)
	{
		rootPath = newRoot;

		refreshAll();
	}

	/**
	 */
	public void addTreeModelListener(TreeModelListener l)
	{
		listeners.add(l);
	}

	/**
	 */
	public Object getChild(Object parent, int index)
	{
		File f = (File)parent;

		if(f.isDirectory())
		{
			if(index >= f.list().length)
				return null;
			else
				return new ViewFile(f, f.list()[index]);
		}
		else
		{
			return null;
		}
	}

	/**
	 */
	public int getChildCount(Object parent)
	{
		File f = (File)parent;

		if(f.isDirectory())
			return f.list().length;
		else
			return 0;
	}

	/**
	 */
	public int getIndexOfChild(Object parent, Object child)
	{
		File f = (File)parent;
		String childname = ((File)child).getName();

		if(f.isDirectory())
		{
			String[] files = f.list();

			for(int i = 0; i < files.length; ++i)
			{
				if(files[i].equals(childname))
					return i;
			}
		}

		return -1;
	}

	/**
	 */
	public Object getRoot()
	{
		return rootPath;
	}

	/**
	 */
	public boolean isLeaf(Object node)
	{
		File f = (File)node;
		return f.isFile();
	}

	/**
	 */
	public void removeTreeModelListener(TreeModelListener l)
	{
		listeners.remove(l);
	}

	public void valueForPathChanged(TreePath path, Object newValue)
	{
		throw new IllegalArgumentException("ASDASD");
	}
}

public class FileBrowser
{
	/*
	private static void add(DefaultMutableTreeNode rootNode, File path)
	{
		String rootpath = path.getAbsolutePath();
		for(String f : path.list())
		{
			File file = new ViewFile(rootpath + "/" + f);
			DefaultMutableTreeNode n = new DefaultMutableTreeNode(file);
			rootNode.add(n);

			if(file.isDirectory())
			{
				add(n, file);
			}
			else if(file.isFile())
			{
			}
		}
	}
	*/

	public static void main(String args[])
	{
		JFrame frame = new JFrame("File Browser");

		JScrollPane scroll = new JScrollPane();

		final JTree tree = new JTree();
		tree.setRootVisible(true);
		tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

		File root = new ViewFile(System.getProperty("user.dir"));

		final FileTreeModel model = new FileTreeModel(root);
		tree.setModel(model);

		tree.addMouseListener(new MouseAdapter()
				{
					private void showPopup(MouseEvent e)
					{
						if(!e.isPopupTrigger())
							return;

						// check if it's a directory, because we can only refresh dirs
						final TreePath path = tree.getPathForLocation(e.getX(), e.getY());
						File f = (File)path.getLastPathComponent();
						if(!f.isDirectory())
							return;

						int row = tree.getRowForLocation(e.getX(), e.getY());
						tree.setSelectionRow(row);
						JPopupMenu m = new JPopupMenu();
						JMenuItem mi = new JMenuItem("Refresh");
						mi.addActionListener(new ActionListener()
							{
								public void actionPerformed(ActionEvent e)
								{
									TreeModelEvent event = new TreeModelEvent(this, path);
									model.fireTreeStructureChanged(event);
								}
							});
						m.add(mi);
						m.show(e.getComponent(), e.getX(), e.getY());
					}

					public void mousePressed(MouseEvent e)
					{
						showPopup(e);
					}

					public void mouseReleased(MouseEvent e)
					{
						showPopup(e);
					}

					public void mouseClicked(MouseEvent e)
					{
						if(e.getClickCount() == 2 && e.getButton() == MouseEvent.BUTTON1)
						{
							int x = e.getX();
							int y = e.getY();

							Object o = tree.getClosestPathForLocation(x, y)
								.getLastPathComponent();

							System.out.println("e " +
								((File)o).getAbsolutePath());
						}
					}
				});

		// add a listener so F5 key will refresh tree
		// TODO: make the refresh method not reset the whole tree is possible
		Object o = new Object();
		tree.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0), o);
		tree.getActionMap().put(o, new AbstractAction()
				{
					public void actionPerformed(ActionEvent e)
					{
						model.refreshAll();
					}
				});

		JButton btnSetRoot = new JButton("Set Root");
		btnSetRoot.addActionListener(new ActionListener()
				{
					public void actionPerformed(ActionEvent e)
					{
						JFileChooser chooser =
							new JFileChooser((File)model.getRoot());

						chooser.setFileSelectionMode(
							JFileChooser.DIRECTORIES_ONLY);

						int res = chooser.showOpenDialog(null);

						if(res != JFileChooser.APPROVE_OPTION)
							return;

						File newRoot = chooser.getSelectedFile();
						model.setRoot(newRoot);
					}
				});

		scroll.setViewportView(tree);

		frame.getContentPane().add(btnSetRoot, BorderLayout.NORTH);
		frame.getContentPane().add(scroll, BorderLayout.CENTER);

		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		//frame.pack();
		frame.setSize(300, 600);
		frame.setVisible(true);

	}
}

