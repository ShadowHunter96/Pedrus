package cz.bbn.cerberus.offer.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.opportunity.ui.OpportunityDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;

public class OfferDetailCardComponent extends AppDetailCardComponent<OfferDto> {

    private final UserService userService;
    private final ListService listService;
    private final AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent;
    private final Button addSign;
    private final boolean readOnly;
    private final OfferDto dto;
    private final SubjectService subjectService;

    public OfferDetailCardComponent(OfferDto dto, SaveAction<OfferDto> saveAction,
                                    boolean readOnly, AppEnv appEnv,
                                    UserService userService, ListService listService,
                                    EntityNewComponentOperation entityNewComponentOperation, Button addSign,
                                    AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent,
                                    SubjectService subjectService) {
        super(dto, saveAction, !readOnly, appEnv, entityNewComponentOperation);
        this.userService = userService;
        this.listService = listService;
        this.areaTechnologySignsBadgeComponent = areaTechnologySignsBadgeComponent;
        this.addSign = addSign;
        this.readOnly = readOnly;
        this.dto = dto;
        this.subjectService = subjectService;
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = Transl.get("Offer")
                .concat(" - ")
                .concat(String.valueOf(getDto().getId()));
        setHeading(heading);
        if (SecurityUtils.hasCustomPermission(DomainEnum.OFFER_DOMAIN_NAME.getValue(),
                dto.getId(), Permission.OFFER_AREA_TECHNOLOGY_SIGN_EDIT.name())) {
            addButton(addSign);
        }
        this.addBackButton(OpportunityDetailView.ROUTE.concat("/").concat(getDto()
                .getOpportunityDto().getId().replace("/", "&ndash")));
        if (isShowSubmitButton()) {
            addSaveButton();
        }
        this.setId(RobotFrameworkVariables.OFFER_DETAIL_CARD_ID.getValue());
        this.setSizeFull();
        setMargin(false);
        setPadding(false);

        OfferDetailComponent labelDetailComponent = new OfferDetailComponent(getDto(), getBinder(), null,
                userService.findUserList(), listService.getSubjectDtoListByOwnCompany(),
                areaTechnologySignsBadgeComponent, readOnly, subjectService.getAllowedCustomers());
        this.add(labelDetailComponent);
    }

}
