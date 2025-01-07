package cz.bbn.cerberus.subject.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectDto;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonAddToGridComponent;
import cz.bbn.cerberus.permission.Permission;

public class SubjectContactPersonTab extends TabSimpleComponent {

    private final DeleteAction deleteAction;
    private final ItemsAction<ContactPersonByObjectDto> itemsAction;
    private final AppEnv appEnv;
    private ContactPersonAddToGridComponent grid;

    public SubjectContactPersonTab(
            DeleteAction deleteAction, ItemsAction<ContactPersonByObjectDto> itemsAction,
            AppEnv appEnv) {
        this.deleteAction = deleteAction;
        this.itemsAction = itemsAction;
        this.appEnv = appEnv;
        initTab();
    }

    private void initTab() {
        setSizeFull();
        grid = new ContactPersonAddToGridComponent(deleteAction, itemsAction, Permission.SUBJECT_EDIT, appEnv);
        grid.setSizeFull();
        loadData();
        add(grid);
    }

    public void loadData() {
        grid.loadData();
    }

    public AppInfiniteGrid<ContactPersonByObjectDto> getGrid() {
        return grid;
    }
}
