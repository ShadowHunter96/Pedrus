package cz.bbn.cerberus.contactperson.ui;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.contactperson.ContactPersonComponentOperation;
import cz.bbn.cerberus.contactperson.ContactPersonService;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonFilterDto;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonFilterComponent;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonGridComponent;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeService;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.ProjectService;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Route(value = ContactPersonView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.CONTACT_PERSON_VIEW)
@Slf4j
public class ContactPersonView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "contact-person-list";

    private final AppEnv appEnv;
    private final ContactPersonService contactPersonService;
    private final ContactPersonTypeService contactPersonTypeService;
    private final ProjectService projectService;
    private final SubjectService subjectService;
    private final ContactPersonComponentOperation contactPersonComponentOperation;
    private final ListService listService;
    private final EntityNewComponentOperation entityNewComponentOperation;

    private ContactPersonGridComponent grid;

    public ContactPersonView(AppEnv appEnv, ContactPersonService contactPersonService,
                             ContactPersonTypeService contactPersonTypeService, ProjectService projectService,
                             SubjectService subjectService,
                             ContactPersonComponentOperation contactPersonComponentOperation,
                             ListService listService, EntityNewComponentOperation entityNewComponentOperation) {
        this.appEnv = appEnv;
        this.contactPersonService = contactPersonService;
        this.contactPersonTypeService = contactPersonTypeService;
        this.projectService = projectService;
        this.subjectService = subjectService;
        this.contactPersonComponentOperation = contactPersonComponentOperation;
        this.listService = listService;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        ContactPersonFilterComponent contactPersonFilterComponent =
                new ContactPersonFilterComponent(getContactPersonTypeDtoList(),
                        listService.getAllowedProjectDtoList(),
                        subjectService.findSubjectAllowedList(), search, params, getHistoryBreadcrumbs());

        grid = new ContactPersonGridComponent(
                getDeleteAction(), appEnv, getItemsAction(contactPersonFilterComponent),
                contactPersonComponentOperation, listService);

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Contact person list"),
                Permission.CONTACT_PERSON_EDIT, Transl.get("Add contact person"),
                entityNewComponentOperation.getNewContactPersonDialogEvent(grid, null),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.setId(RobotFrameworkVariables.CONTACT_PERSON_VIEW_CARD_ID.getValue());
        card.add(contactPersonFilterComponent);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        card.add(grid);

        add(card);
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            contactPersonFilterComponent.fillUrl();
        });
    }

    private List<ContactPersonTypeDto> getContactPersonTypeDtoList() {
        return contactPersonTypeService.findAll();
    }

    private ItemsAction<ContactPersonDto> getItemsAction(ContactPersonFilterComponent filterComponent) {
        return (query, orderList) -> {
            ContactPersonFilterDto filter = filterComponent.getContactPersonFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return contactPersonService.findContactPersonDtoPage(filter);
        };
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                contactPersonService.deleteContactPerson(id);
                SuccessNotification.showSavingSuccess(appEnv);
                grid.loadData();
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    @Override
    public void onAttach(AttachEvent event) {
        if (grid != null) {
            grid.loadData();
        }
        super.onAttach(event);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
