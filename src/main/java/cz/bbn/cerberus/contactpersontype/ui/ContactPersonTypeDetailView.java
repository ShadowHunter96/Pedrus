package cz.bbn.cerberus.contactpersontype.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeComponentOperation;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeService;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.ui.components.ContactPersonTypeDetailComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import lombok.extern.slf4j.Slf4j;

@Route(value = ContactPersonTypeDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.CONTACT_PERSON_TYPE_VIEW)
@Slf4j
public class ContactPersonTypeDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "contact-person-type-detail";

    private final ContactPersonTypeService contactPersonTypeService;
    private final ContactPersonTypeComponentOperation contactPersonTypeComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public ContactPersonTypeDetailView(ContactPersonTypeService contactPersonTypeService,
                                       ContactPersonTypeComponentOperation contactPersonTypeComponentOperation,
                                       AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.contactPersonTypeService = contactPersonTypeService;
        this.contactPersonTypeComponentOperation = contactPersonTypeComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(ContactPersonTypeDto dto) {
        removeAll();
        setSizeFull();
        ContactPersonTypeDetailComponent contactPersonDetailTypeComponent =
                new ContactPersonTypeDetailComponent(
                        dto,
                        contactPersonTypeComponentOperation.getSaveAction(null),
                        SecurityUtils.hasPermission(Permission.CONTACT_PERSON_EDIT),
                        appEnv, entityNewComponentOperation);
        add(contactPersonDetailTypeComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                ContactPersonTypeDto dto = contactPersonTypeService.getContactPersonType(param);
                refreshBreadcrumbText(dto.getId());
                initView(dto);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }
}
