package cz.bbn.cerberus.contract.ui.component.tab;

import com.vaadin.flow.component.combobox.ComboBox;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectDto;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonAddToGridComponent;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.permission.Permission;

import java.util.List;

public class ContractContactPersonTab extends TabSimpleComponent {

    private final DeleteAction deleteAction;
    private final ItemsAction<ContactPersonByObjectDto> itemsAction;
    private final AppEnv appEnv;
    private final ComboBox<String> addPersonComboBox = new ComboBox<>();
    private final ContractComponentOperation contractComponentOperation;
    private ContactPersonAddToGridComponent grid;
    private ContractDto dto;

    public ContractContactPersonTab(
            DeleteAction deleteAction, ItemsAction<ContactPersonByObjectDto> itemsAction,
            ContractDto dto, AppEnv appEnv, ContractComponentOperation contractComponentOperation) {
        this.deleteAction = deleteAction;
        this.itemsAction = itemsAction;
        this.appEnv = appEnv;
        this.contractComponentOperation = contractComponentOperation;
        this.dto = dto;
        initTab();
    }

    private void initTab() {
        setSizeFull();
        grid = new ContactPersonAddToGridComponent(deleteAction, itemsAction, Permission.CONTRACT_EDIT, appEnv);
        grid.setSizeFull();
        add(grid);
        loadData();
    }

    private void setContactPersonData() {
        List<String> idList = contractComponentOperation.getContactPersonsToAdd(dto);
        addPersonComboBox.setItems(idList);
        if (!idList.isEmpty()) {
            addPersonComboBox.setValue(idList.get(0));
        } else {
            addPersonComboBox.setValue(TextValues.EMPTY_VALUE);
        }
    }

    public void loadData() {
        grid.loadData();
        setContactPersonData();
    }

    public AppInfiniteGrid<ContactPersonByObjectDto> getGrid() {
        return grid;
    }
}
