package cz.bbn.cerberus.contactperson.ui.components;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectDto;
import cz.bbn.cerberus.contactperson.ui.ContactPersonDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class ContactPersonAddToGridComponent extends AppInfiniteGrid<ContactPersonByObjectDto> {

    private final Permission editPermission;
    private String header = "";

    public ContactPersonAddToGridComponent(DeleteAction deleteAction, ItemsAction<ContactPersonByObjectDto> itemsAction,
                                           Permission editPermission, AppEnv appEnv) {
        super(deleteAction, appEnv, itemsAction);
        this.editPermission = editPermission;
        initGrid();
    }

    public ContactPersonAddToGridComponent(DeleteAction deleteAction, ItemsAction<ContactPersonByObjectDto> itemsAction,
                                           Permission editPermission, String header, AppEnv appEnv) {
        super(deleteAction, appEnv, itemsAction);
        this.editPermission = editPermission;
        this.header = header;
        initGrid();
    }

    private void initGrid() {
        setSizeFull();
        addColumn(ContactPersonByObjectDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(ContactPersonByObjectDto::getFirstName).setHeader(Transl.get("First name"))
                .setSortable(true).setKey("firstName");
        addColumn(ContactPersonByObjectDto::getLastName).setHeader(Transl.get("Last name"))
                .setSortable(true).setKey("lastName");
        addColumn(ContactPersonByObjectDto::getEmail).setHeader(Transl.get("Email")).setSortable(true).setKey("email");
        addColumn(contactPersonByObjectDto -> AppUtils.phoneFormat(contactPersonByObjectDto.getPhonePrefixDto(),
                contactPersonByObjectDto.getPhone())).setHeader(Transl.get("Phone"))
                .setSortable(true).setKey("phone");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(e ->
                UI.getCurrent().navigate(ContactPersonDetailView.ROUTE + "/" + e.getItem().getId()));
    }

    private HorizontalLayout getGridButtons(ContactPersonByObjectDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("",
                "Are you sure you want to remove contact person {0} ?", "Remove contact person");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                null, editPermission, clickedItem.getId(), "", ContactPersonDetailView.ROUTE,
                DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue(), false);
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }
}
