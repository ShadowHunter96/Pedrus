package cz.bbn.cerberus.contactperson.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.ContactPersonComponentOperation;
import cz.bbn.cerberus.contactperson.ContactPersonService;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonTabsComponent;
import cz.bbn.cerberus.contactperson.ui.components.tab.ContactPersonDetailTab;
import cz.bbn.cerberus.contactperson.ui.components.tab.ContactPersonDocumentTab;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.ui.component.NoteComponent;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.phoneprefix.PhonePrefixService;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@Route(value = ContactPersonDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.CONTACT_PERSON_VIEW)
@Slf4j
public class ContactPersonDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "contact-person-detail";

    private final ContactPersonService contactPersonService;
    private final NoteComponentOperation noteComponentOperation;
    private final DocumentComponentOperation documentComponentOperation;
    private final AppEnv appEnv;
    private final ContactPersonComponentOperation contactPersonComponentOperation;
    private final PhonePrefixService phonePrefixService;
    private final ListService listService;
    private final EntityNewComponentOperation entityNewComponentOperation;

    private boolean readOnly = true;

    public ContactPersonDetailView(ContactPersonService contactPersonService,
                                   NoteComponentOperation noteComponentOperation,
                                   DocumentComponentOperation documentComponentOperation, AppEnv appEnv,
                                   ContactPersonComponentOperation contactPersonComponentOperation,
                                   PhonePrefixService phonePrefixService, ListService listService,
                                   EntityNewComponentOperation entityNewComponentOperation) {
        this.contactPersonService = contactPersonService;
        this.noteComponentOperation = noteComponentOperation;
        this.documentComponentOperation = documentComponentOperation;
        this.appEnv = appEnv;
        this.contactPersonComponentOperation = contactPersonComponentOperation;
        this.phonePrefixService = phonePrefixService;
        this.listService = listService;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(ContactPersonDto dto) {
        removeAll();
        setSizeFull();
        String heading = dto.getId() == null ? Transl.get("New contact person") :
                Transl.get("Contact person")
                        .concat(" - ")
                        .concat(dto.getName());

        List<TabEntry> tabList = new ArrayList<>();
        tabList.add(
                new TabEntry(
                        Transl.get("Contact person detail"), new ContactPersonDetailTab(
                        dto, contactPersonComponentOperation.getSaveAction(null),
                        contactPersonComponentOperation.getContactPersonTypeDtoList(), appEnv,
                        phonePrefixService.findAllPhonePrefixDtoList(), false, readOnly)
                )
        );

        Button addDocument = VaadinComponents.getNewButton(Transl.get("Add new document"), false);
        if (dto.getId() != null && SecurityUtils.hasPermission(Permission.DOCUMENT_VIEW)) {
            tabList.add(new TabEntry(Transl.get("Documents"),
                    new ContactPersonDocumentTab(
                            documentComponentOperation, dto, !readOnly, appEnv, addDocument, listService
                    ),
                    Permission.CONTACT_PERSON_DOCUMENT_VIEW));
        }

        ContactPersonTabsComponent tabsComponent =
                new ContactPersonTabsComponent(
                        heading, tabList,
                        SecurityUtils.hasPermission(Permission.CONTACT_PERSON_EDIT) && !readOnly,
                        addDocument, entityNewComponentOperation
                );

        tabsComponent.showSubjectsMenu(contactPersonComponentOperation.getContactPersonSubjectDtoList(dto.getId()));
        tabsComponent.addNewEntitySlideTab(
                new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES)
        );
        if (dto.getId() != null && SecurityUtils.hasPermission(Permission.CONTACT_PERSON_NOTE_VIEW)) {
            Checkbox showArchived = new Checkbox(Transl.get("Show also archived"));
            CountIntIndicator noteIndicator = new CountIntIndicator(
                    noteComponentOperation.getNoteCountByTypeAndObjectId(NoteTypeEnum.CONTACT_PERSON, dto.getId())
            );
            NoteComponent noteComponent = new NoteComponent(noteComponentOperation, showArchived,
                    dto.getId(), appEnv, NoteTypeEnum.CONTACT_PERSON, Permission.CONTACT_PERSON_NOTE_VIEW,
                    Permission.CONTACT_PERSON_NOTE_EDIT,
                    true, !readOnly, noteIndicator);
            tabsComponent.addNoteSlideTab(noteComponent, noteIndicator);
        }

        add(tabsComponent);
    }

    private void setParam(String param) throws SystemException {
        if (SecurityUtils.hasCustomPermission(
                DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue(), param, Permission.CONTACT_PERSON_VIEW.name())) {
            ContactPersonDto dto = contactPersonService.getContactPerson(param);
            refreshBreadcrumbText(AppUtils.cutText(dto.getFirstName(), 2, false)
                    .concat(AppUtils.cutText(dto.getLastName(), 2, false)));
            if (SecurityUtils.hasCustomPermission(
                    DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue(), param,
                    Permission.CONTACT_PERSON_EDIT.name())) {
                readOnly = false;
            }
            initView(dto);
        } else {
            ErrorNotification.show(ErrorCode.VIEW_PERMISSION_MISSING.getError(), appEnv);
            UI.getCurrent().access(
                    () -> UI.getCurrent().getPage().fetchCurrentURL(e -> UI.getCurrent().navigate(e.getPath()))
            );
        }
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                setParam(param);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }
}
