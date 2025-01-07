package cz.bbn.cerberus.opportunity.ui;

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
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.opportunity.OpportunityService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDtoFilter;
import cz.bbn.cerberus.opportunity.dto.OpportunitySimpleDto;
import cz.bbn.cerberus.opportunity.ui.component.OpportunityFilterComponent;
import cz.bbn.cerberus.opportunity.ui.component.OpportunityGridComponent;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;

@Route(value = OpportunityView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.OPPORTUNITY_VIEW)
@Slf4j
public class OpportunityView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "opportunity-list";

    private final AppEnv appEnv;
    private final OpportunityService opportunityService;
    private final ListService listService;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final UserService userService;

    public OpportunityView(AppEnv appEnv, OpportunityService opportunityService, ListService listService,
                           EntityNewComponentOperation entityNewComponentOperation, UserService userService) {
        this.appEnv = appEnv;
        this.opportunityService = opportunityService;
        this.listService = listService;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.userService = userService;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        OpportunityFilterComponent opportunityFilterComponent = new OpportunityFilterComponent(
                search, listService.getSubjectDtoListByCustomer(), params, getHistoryBreadcrumbs());

        OpportunityGridComponent grid = new OpportunityGridComponent(getDeleteAction(), appEnv,
                getItemsAction(opportunityFilterComponent), listService.getSubjectMap(), userService.getOwnerMap());

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Opportunity list"),
                Permission.OPPORTUNITY_EDIT, Transl.get("Add opportunity"),
                entityNewComponentOperation.getNewOpportunityEvent(grid, null), entityNewComponentOperation,
                NoteTypeEnum.ANY, ObjectType.ANY);
        card.setId(RobotFrameworkVariables.OPPORTUNITY_VIEW_CARD_ID.getValue());
        card.add(opportunityFilterComponent);
        card.add(grid);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            opportunityFilterComponent.fillUrl();
        });
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }

    private ItemsAction<OpportunitySimpleDto> getItemsAction(OpportunityFilterComponent opportunityFilterComponent) {
        return (query, orderList) -> {
            OpportunityDtoFilter filter = opportunityFilterComponent.getOpportunityDtoFilter();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return opportunityService.findOpportunityDtoPage(filter);
        };
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                opportunityService.delete(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
