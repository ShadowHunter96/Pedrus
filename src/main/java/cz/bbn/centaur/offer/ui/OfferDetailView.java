package cz.bbn.cerberus.offer.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.ui.component.NoteComponent;
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.offer.OfferService;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.ui.component.OfferTabsComponent;
import cz.bbn.cerberus.offer.ui.component.tabs.OfferDetailTab;
import cz.bbn.cerberus.offer.ui.component.tabs.OfferEmailTab;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.ui.component.TaskSlideTabComponent;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@Route(value = OfferDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.OFFER_VIEW)
@Slf4j
public class OfferDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "offer-detail";

    private final OfferService offerService;
    private final AppEnv appEnv;
    private final OfferComponentOpperation offerComponentOpperation;
    private final AreaTechnologyComponentOperation areaTechnologyComponentOperation;
    private final UserService userService;
    private final ListService listService;
    private final TaskComponentOperation taskComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final EmailComponentOperations emailComponentOperations;
    private final SubjectService subjectService;
    private final NoteComponentOperation noteComponentOperation;

    private boolean readOnly;

    public OfferDetailView(OfferService offerService, AppEnv appEnv, OfferComponentOpperation offerComponentOpperation,
                           AreaTechnologyComponentOperation areaTechnologyComponentOperation, UserService userService,
                           ListService listService, TaskComponentOperation taskComponentOperation,
                           EntityNewComponentOperation entityNewComponentOperation,
                           EmailComponentOperations emailComponentOperations, SubjectService subjectService,
                           NoteComponentOperation noteComponentOperation) {
        this.offerService = offerService;
        this.appEnv = appEnv;
        this.offerComponentOpperation = offerComponentOpperation;
        this.areaTechnologyComponentOperation = areaTechnologyComponentOperation;
        this.userService = userService;
        this.listService = listService;
        this.taskComponentOperation = taskComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.emailComponentOperations = emailComponentOperations;
        this.subjectService = subjectService;
        this.noteComponentOperation = noteComponentOperation;
    }

    private void initView(OfferDto dto) {
        removeAll();
        AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent =
                new AreaTechnologySignsBadgeComponent(areaTechnologyComponentOperation, ObjectType.OFFER, dto.getId());

        List<TabEntry> tabEntryList = new ArrayList<>();

        tabEntryList.add(new TabEntry(Transl.get("Offer detail"),
                new OfferDetailTab(appEnv,
                        areaTechnologyComponentOperation, userService, listService,
                        readOnly, dto,
                        offerComponentOpperation.getSaveAction(null), areaTechnologySignsBadgeComponent,
                        subjectService)));

        if (SecurityUtils.hasCustomPermission(DomainEnum.OFFER_DOMAIN_NAME.getValue(),
                dto.getId(), Permission.OFFER_EMAIL_VIEW.name())) {
            OfferEmailTab contractMailTab = new OfferEmailTab(dto, emailComponentOperations, appEnv);
            tabEntryList.add(new TabEntry(Transl.get("Email list"),
                    contractMailTab));
        }

        Button addAreaTechnologySign = VaadinComponents.getNewButton(Transl.get("Add new sign"), false);
        addAreaTechnologySign.addClickListener(buttonClickEvent ->
                areaTechnologyComponentOperation.getAreaTechnologySignEvent(ObjectType.OFFER, dto.getId(),
                        areaTechnologySignsBadgeComponent).onComponentEvent(buttonClickEvent));

        String heading = Transl.get("Offer")
                .concat(" - ")
                .concat(String.valueOf(dto.getId()));
        OfferTabsComponent tabsComponent = new OfferTabsComponent(heading, tabEntryList,
                entityNewComponentOperation, addAreaTechnologySign, !readOnly, dto);

        TaskSlideTabComponent taskSlideTabComponent = new TaskSlideTabComponent(
                taskComponentOperation, appEnv, ObjectType.OFFER,
                dto.getId(), dto.getSubjectDto(), listService);
        tabsComponent.addEventSlideTab(taskSlideTabComponent);
        tabsComponent.addNewEntitySlideTab(
                new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));

        if (dto.getId() != null && SecurityUtils.hasPermission(Permission.OFFER_NOTE_VIEW)) {
            Checkbox showArchived = new Checkbox(Transl.get("Show also archived"));
            boolean canViewCustomPermission = SecurityUtils.hasCustomPermission(
                    DomainEnum.OFFER_DOMAIN_NAME.getValue(), dto.getId(),
                    Permission.OFFER_NOTE_VIEW.name());
            boolean canEditCustomPermission = SecurityUtils.hasCustomPermission(
                    DomainEnum.OFFER_DOMAIN_NAME.getValue(), dto.getId(),
                    Permission.OFFER_NOTE_EDIT.name());
            CountIntIndicator noteIndicator = new CountIntIndicator(
                    noteComponentOperation.getNoteCountByTypeAndObjectId(NoteTypeEnum.OFFER, dto.getId()));
            NoteComponent noteComponent = new NoteComponent(noteComponentOperation, showArchived,
                    dto.getId(), appEnv,
                    NoteTypeEnum.OFFER,
                    Permission.OFFER_NOTE_VIEW, Permission.OFFER_NOTE_EDIT,
                    canViewCustomPermission,
                    canEditCustomPermission, noteIndicator);
            tabsComponent.addNoteSlideTab(noteComponent, noteIndicator);
        }

        add(tabsComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, String param) {
        try {
            if (param != null && SecurityUtils.hasCustomPermission(DomainEnum.OFFER_DOMAIN_NAME.getValue(),
                    param.replace("&ndash", "/"), Permission.OFFER_VIEW.name())) {
                OfferDto dto = offerService.getOfferDto(param.replace("&ndash", "/"));
                refreshBreadcrumbText(dto.getName());
                readOnly = !SecurityUtils.hasCustomPermission(
                        DomainEnum.OFFER_DOMAIN_NAME.getValue(), dto.getId(), Permission.OFFER_EDIT.name());
                initView(dto);
            } else {
                ErrorNotification.show(Transl.get(ErrorCode.VIEW_PERMISSION_MISSING.getError()), appEnv);
                UI.getCurrent().access(
                        () -> UI.getCurrent().getPage().fetchCurrentURL(e -> UI.getCurrent().navigate(e.getPath()))
                );
            }
        } catch (SystemException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
    }
}
