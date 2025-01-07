package cz.bbn.cerberus.management.ui;


import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.component.orderedlayout.FlexLayout;
import com.vaadin.flow.component.radiobutton.RadioButtonGroup;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCard;
import cz.bbn.cerberus.commons.component.ui.appcard.AppPieGraphCard;
import cz.bbn.cerberus.commons.component.ui.layouts.CenteredHorizontalLayout;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enums.QuarterDateFilter;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.management.ManagementComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.ProjectState;
import cz.bbn.cerberus.project.ui.ProjectView;
import cz.bbn.cerberus.subject.ui.SubjectView;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDate;


@Route(value = ManagementDashboardView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.MANAGEMENT_DASHBOARD)
@Slf4j
public class ManagementDashboardView extends AppView {

    public static final String ROUTE = "management-dashboard";

    private final EntityNewComponentOperation entityNewComponentOperation;
    private final ManagementComponentOperation managementComponentOperation;
    private final ListService listService;

    public ManagementDashboardView(EntityNewComponentOperation entityNewComponentOperation,
                                   ManagementComponentOperation managementComponentOperation, ListService listService) {
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.managementComponentOperation = managementComponentOperation;
        this.listService = listService;
        initView();
    }

    private void initView(){
        setSizeFull();
        AppCard card = new AppCard(entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        card.addToHeader(new H2(Transl.get("Management dashboard")));

        FlexLayout pieGraphFlexLayout = new FlexLayout();
        pieGraphFlexLayout.setJustifyContentMode(JustifyContentMode.CENTER);

        CenteredHorizontalLayout centeredCircleGraphHorizontalLayout = new CenteredHorizontalLayout();
        pieGraphFlexLayout.setFlexWrap(FlexLayout.FlexWrap.WRAP);
        pieGraphFlexLayout.addClassName("flex-layout");
        centeredCircleGraphHorizontalLayout.add(pieGraphFlexLayout);

        LocalDate actualDate = LocalDate.now();
        DatePicker from = VaadinComponents.getDatePicker(Transl.get("From"), AppUtils.getQuarterFrom(actualDate));
        DatePicker to = VaadinComponents.getDatePicker(Transl.get("To"), AppUtils.getQuarterTo(actualDate));

        RadioButtonGroup<QuarterDateFilter> quarterDateFilter = new RadioButtonGroup<>();
        quarterDateFilter.setItems(QuarterDateFilter.values());
        quarterDateFilter.setItemLabelGenerator(actual -> Transl.get(actual.name()));
        quarterDateFilter.addValueChangeListener(event -> {
            AppUtils.setDatesByQuarterFilter(event.getValue(), from, to);
            managementComponentOperation.reloadData(pieGraphFlexLayout);
        });
        quarterDateFilter.setValue(AppUtils.getQuarterDateFilterValue(actualDate));

        from.addValueChangeListener(event -> {
            if(event.getValue() != null && to.getValue() != null && event.isFromClient()) {
                managementComponentOperation.reloadData(pieGraphFlexLayout);
                quarterDateFilter.setValue(QuarterDateFilter.OWN_FILTER);
            }
        });

        to.addValueChangeListener(event -> {
            if(event.getValue() != null && from.getValue() != null && event.isFromClient()) {
                managementComponentOperation.reloadData(pieGraphFlexLayout);
                quarterDateFilter.setValue(QuarterDateFilter.OWN_FILTER);
            }
        });

        CenteredHorizontalLayout dateLayout = new CenteredHorizontalLayout();
        dateLayout.add(from, to);

        CenteredHorizontalLayout radioGroupLayout = new CenteredHorizontalLayout();
        radioGroupLayout.add(quarterDateFilter);

        card.add(dateLayout, radioGroupLayout);

        addAppCircleGraphCards(pieGraphFlexLayout, from, to);
        card.add(centeredCircleGraphHorizontalLayout);
        add(card);
    }

    private void addAppCircleGraphCards(FlexLayout flexCountLayout, DatePicker from, DatePicker to){
        flexCountLayout.add(new AppPieGraphCard(Transl.get("Customers"),
                managementComponentOperation.getCustomerByUserMapAction(listService.getUserDtoList()),
                SubjectView.ROUTE.concat("/&onlyCustomer=true")));
        flexCountLayout.add(new AppPieGraphCard(Transl.get("Opportunities"),
                managementComponentOperation.getOpportunitiesByUserMapAction(listService.getUserDtoList(), from, to),
                managementComponentOperation.getOpportunityRoute(from, to, false)));
        flexCountLayout.add(new AppPieGraphCard(Transl.get("Offers"),
                managementComponentOperation.getOffersByUserMapAction(listService.getUserDtoList(), from, to),
                managementComponentOperation.getOfferRoute(from, to, false)));
        flexCountLayout.add(new AppPieGraphCard(Transl.get("Contracts"),
                managementComponentOperation.getContractsByUserMapAction(listService.getUserDtoList(), from, to),
                managementComponentOperation.getContractRoute(from, to,false)));
        flexCountLayout.add(new AppPieGraphCard(Transl.get("Invoices"),
                managementComponentOperation.getInvoicesByUserMapAction(listService.getUserDtoList(), from, to),
                managementComponentOperation.getInvoiceRoute(from, to, false)));
        flexCountLayout.add(new AppPieGraphCard(Transl.get("Projects"),
                managementComponentOperation.getProjectsByUserMapAction(listService.getUserDtoList()),
                ProjectView.ROUTE + "/&state=" + ProjectState.REALIZE));
    }

}
