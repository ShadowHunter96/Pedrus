package cz.bbn.cerberus.offer.ui.component.tabs;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabDtoComponent;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.ui.component.OfferDetailComponent;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;

public class OfferDetailTab extends TabDtoComponent<OfferDto> {

    private final AreaTechnologyComponentOperation areaTechnologyComponentOperation;
    private final AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent;
    private final UserService userService;
    private final ListService listService;
    private final boolean readOnly;
    private final OfferDto dto;
    private final SubjectService subjectService;

    public OfferDetailTab(AppEnv appEnv,
                          AreaTechnologyComponentOperation areaTechnologyComponentOperation, UserService userService,
                          ListService listService, boolean readOnly,
                          OfferDto dto, SaveAction<OfferDto> saveAction,
                          AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent,
                          SubjectService subjectService) {
        super(dto, saveAction, appEnv);
        this.dto = dto;
        this.areaTechnologyComponentOperation = areaTechnologyComponentOperation;
        this.userService = userService;
        this.listService = listService;
        this.readOnly = readOnly;
        this.areaTechnologySignsBadgeComponent = areaTechnologySignsBadgeComponent;
        this.subjectService = subjectService;
        initTab();
    }


    @Override
    protected void initTab() {
        removeAll();

        this.setId(RobotFrameworkVariables.OFFER_DETAIL_CARD_ID.getValue());
        this.setSizeFull();
        this.setMargin(false);
        this.setPadding(false);
        AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent =
                new AreaTechnologySignsBadgeComponent(areaTechnologyComponentOperation, ObjectType.OFFER, dto.getId());

        Button addAreaTechnologySign = VaadinComponents.getNewButton(Transl.get("Add new sign"), false);
        addAreaTechnologySign.addClickListener(buttonClickEvent ->
                areaTechnologyComponentOperation.getAreaTechnologySignEvent(ObjectType.OFFER, dto.getId(),
                        areaTechnologySignsBadgeComponent).onComponentEvent(buttonClickEvent));

        OfferDetailComponent offerDetailComponent = new OfferDetailComponent(getDto(), getBinder(),
                null, userService.findUserList(), listService.getSubjectDtoListByOwnCompany(),
                areaTechnologySignsBadgeComponent, readOnly, subjectService.getAllowedCustomers());
        VerticalLayout mainHorizontalLayout = new VerticalLayout();
        mainHorizontalLayout.setSizeFull();
        mainHorizontalLayout.add(offerDetailComponent);

        this.add(mainHorizontalLayout);
    }

    @Override
    public void loadTab() {
        areaTechnologySignsBadgeComponent.loadData();
    }
}
