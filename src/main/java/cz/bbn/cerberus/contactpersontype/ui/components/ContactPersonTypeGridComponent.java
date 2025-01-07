package cz.bbn.cerberus.contactpersontype.ui.components;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.ChangeWarningDialog;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactpersontype.dto.ChangeContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.ui.ContactPersonTypeDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class ContactPersonTypeGridComponent extends AppInfiniteGrid<ContactPersonTypeDto> {

    private final ListAction<String> listAction;
    private final ListAction<ContactPersonTypeDto> listActionForChange;
    private final SaveAction<String> saveActionChange;

    public ContactPersonTypeGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                          ItemsAction<ContactPersonTypeDto> itemsAction,
                                          ListAction<String> listAction,
                                          ListAction<ContactPersonTypeDto> listActionForChange,
                                          SaveAction<String> saveActionChange) {
        super(deleteAction, appEnv, itemsAction);
        this.listAction = listAction;
        this.listActionForChange = listActionForChange;
        this.saveActionChange = saveActionChange;
        initGrid();
    }

    private void initGrid() {
        setSizeFull();
        addColumn(ContactPersonTypeDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(ContactPersonTypeDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(contactPersonTypeDto -> VaadinComponents
                .getCheckUncheckLayoutNullTrue(contactPersonTypeDto.getAllowed())))
                .setHeader(Transl.get("Allowed"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM)
                .setFlexGrow(0)
                .setSortable(true).setKey("allowed");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }


    private HorizontalLayout getGridButtons(ContactPersonTypeDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_TYPE_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, clickedItem.getId());
            delete.addClickListener(buttonClickEvent -> {
                List<String> contactPersonList = listAction.getList(clickedItem.getId());
                if (contactPersonList.isEmpty()) {
                    DeleteConfirmDialog deleteConfirmDialog =
                            new DeleteConfirmDialog(this, clickedItem.getId(), Transl.get(
                                    "Are you sure you want to delete contact person type {0} ",
                                    clickedItem.getId()), getAppEnv(), true);
                    deleteConfirmDialog.open();
                } else {
                    ChangeWarningDialog warningListDialog = getChangeWarningDialog(clickedItem, contactPersonList);
                    warningListDialog.open();
                }
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete contact person type"));
            buttons.add(delete);
        }

        return buttons;
    }

    private ChangeWarningDialog getChangeWarningDialog(ContactPersonTypeDto clickedItem,
                                                       List<String> contactPersonList) {
        ComboBox<ContactPersonTypeDto> contactPersonTypes = new ComboBox<>(Transl.get("Change type to"));
        contactPersonTypes.setItems(listActionForChange.getList(clickedItem.getId()));
        contactPersonTypes.setItemLabelGenerator(ContactPersonTypeDto::getName);
        Binder<ChangeContactPersonTypeDto> binder = new Binder<>();
        binder.forField(contactPersonTypes).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).bind(
                ChangeContactPersonTypeDto::getContactPersonTypeDto,
                ChangeContactPersonTypeDto::setContactPersonTypeDto);
        binder.setBean(new ChangeContactPersonTypeDto());

        Button changeButton = VaadinComponents.getChangeButton(Transl.get("Change and delete"));

        ChangeWarningDialog warningDialog = new ChangeWarningDialog(contactPersonList, contactPersonTypes, changeButton,
                "Delete contact person type", "The contact person type is used", "Contact persons");

        changeButton.setDisableOnClick(true);
        changeButton.addClickListener(buttonClickEvent -> {
            if (binder.validate().isOk()) {
                saveActionChange.saveItem(contactPersonTypes.getValue().getId(), clickedItem.getId());
                this.loadData();
                warningDialog.close();
            }
            changeButton.setEnabled(true);
        });
        return warningDialog;
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(ContactPersonTypeDetailView.ROUTE + "/" + code);
    }
}
