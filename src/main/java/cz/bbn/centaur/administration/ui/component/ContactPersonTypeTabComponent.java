package cz.bbn.cerberus.administration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.ContactPersonService;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeComponentOperation;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeService;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.ui.components.ContactPersonTypeGridComponent;
import cz.bbn.cerberus.contactpersontype.ui.components.ContactPersonTypeNewDialog;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ContactPersonTypeTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 1;

    private final AppEnv appEnv;
    private final ContactPersonTypeService contactPersonTypeService;
    private final ContactPersonService contactPersonService;
    private final ContactPersonTypeComponentOperation contactPersonTypeComponentOperation;

    private ContactPersonTypeGridComponent contactPersonTypeGridComponent;

    public ContactPersonTypeTabComponent(AppEnv appEnv, ContactPersonTypeService contactPersonTypeService,
                                         ContactPersonService contactPersonService,
                                         ContactPersonTypeComponentOperation contactPersonTypeComponentOperation) {
        this.appEnv = appEnv;
        this.contactPersonTypeService = contactPersonTypeService;
        this.contactPersonService = contactPersonService;
        this.contactPersonTypeComponentOperation = contactPersonTypeComponentOperation;
        initComponent();
    }

    public void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.CONTACT_PERSON_TYPE_TAB_CARD_ID.getValue());

        contactPersonTypeGridComponent =
                new ContactPersonTypeGridComponent(getDeleteAction(), appEnv, getItemsAction(),
                        getListAction(), getListActionForChange(), getSaveActionChange());
        contactPersonTypeGridComponent.loadData();
        add(contactPersonTypeGridComponent);

    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_TYPE_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add contact person type"));
            addNew.addClickListener(e ->
                    new ContactPersonTypeNewDialog(contactPersonTypeGridComponent, contactPersonTypeComponentOperation)
                            .open());
            return addNew;
        }
        return null;
    }

    private ListAction<String> getListAction() {
        return contactPersonService::getUsedContactPersonType;
    }

    private ListAction<ContactPersonTypeDto> getListActionForChange() {
        return contactPersonTypeService::findAllEnabled;
    }

    private ItemsAction<ContactPersonTypeDto> getItemsAction() {
        return (query, orderList) ->
                contactPersonTypeService.findContactPersonTypeDtoPage(query.getPage(), query.getPageSize(), orderList);
    }

    private SaveAction<String> getSaveActionChange() {
        return (newValue, oldValue) -> {
            try {
                contactPersonService.changeContactPersonType(newValue, oldValue);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }


    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                contactPersonTypeService.deleteContactPersonType(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
