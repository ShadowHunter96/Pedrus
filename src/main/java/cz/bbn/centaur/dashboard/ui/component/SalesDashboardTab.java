package cz.bbn.cerberus.dashboard.ui.component;

import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.orderedlayout.FlexLayout;
import com.vaadin.flow.component.radiobutton.RadioButtonGroup;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppBarGraphCard;
import cz.bbn.cerberus.commons.component.ui.appcard.AppNumberCard;
import cz.bbn.cerberus.commons.component.ui.layouts.CenteredHorizontalLayout;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enums.QuarterDateFilter;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dashboard.SalesDashboardComponentOperation;
import cz.bbn.cerberus.management.ManagementComponentOperation;
import cz.bbn.cerberus.project.ProjectState;
import cz.bbn.cerberus.project.ui.ProjectView;
import cz.bbn.cerberus.subject.ui.SubjectView;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDate;

@Slf4j
public class SalesDashboardTab extends TabSimpleComponent {

    private final SalesDashboardComponentOperation salesDashboardComponentOperation;
    private final ManagementComponentOperation managementComponentOperation;

    public SalesDashboardTab(SalesDashboardComponentOperation salesDashboardComponentOperation,
                             ManagementComponentOperation managementComponentOperation) {
        this.salesDashboardComponentOperation = salesDashboardComponentOperation;
        this.managementComponentOperation = managementComponentOperation;
        initView();
    }

    private void initView() {
        setSizeFull();

        CenteredHorizontalLayout barLayout = new CenteredHorizontalLayout();
        CenteredHorizontalLayout centeredHorizontalLayout = new CenteredHorizontalLayout();
        FlexLayout countFlexLayout = new FlexLayout();
        countFlexLayout.setJustifyContentMode(JustifyContentMode.CENTER);

        CenteredHorizontalLayout centeredCountHorizontalLayout = new CenteredHorizontalLayout();
        countFlexLayout.setFlexWrap(FlexLayout.FlexWrap.WRAP);
        countFlexLayout.addClassName("flex-layout");
        centeredCountHorizontalLayout.add(countFlexLayout);

        LocalDate actualDate = LocalDate.now();
        DatePicker from = VaadinComponents.getDatePicker(Transl.get("From"), AppUtils.getQuarterFrom(actualDate));
        DatePicker to = VaadinComponents.getDatePicker(Transl.get("To"), AppUtils.getQuarterTo(actualDate));

        RadioButtonGroup<QuarterDateFilter> quarterDateFilter = new RadioButtonGroup<>();
        quarterDateFilter.setItems(QuarterDateFilter.values());
        quarterDateFilter.setItemLabelGenerator(actual -> Transl.get(actual.name()));
        quarterDateFilter.addValueChangeListener(event -> {
            AppUtils.setDatesByQuarterFilter(event.getValue(), from, to);
            salesDashboardComponentOperation.reloadData(countFlexLayout, barLayout);
        });
        quarterDateFilter.setValue(AppUtils.getQuarterDateFilterValue(actualDate));

        from.addValueChangeListener(event -> {
            if (event.getValue() != null && to.getValue() != null && event.isFromClient()) {
                salesDashboardComponentOperation.reloadData(countFlexLayout, barLayout);
                quarterDateFilter.setValue(QuarterDateFilter.OWN_FILTER);
            }
        });

        to.addValueChangeListener(event -> {
            if (event.getValue() != null && from.getValue() != null && event.isFromClient()) {
                salesDashboardComponentOperation.reloadData(countFlexLayout, barLayout);
                quarterDateFilter.setValue(QuarterDateFilter.OWN_FILTER);
            }
        });

        centeredHorizontalLayout.add(from, to);

        CenteredHorizontalLayout radioGroupLayout = new CenteredHorizontalLayout();
        radioGroupLayout.add(quarterDateFilter);

        add(centeredHorizontalLayout, radioGroupLayout);

        addAppNumberCards(countFlexLayout, from, to);
        addAppBarCards(barLayout, from, to);
        add(centeredCountHorizontalLayout, barLayout);
    }

    private void addAppNumberCards(FlexLayout flexCountLayout, DatePicker from, DatePicker to) {
        flexCountLayout.add(new AppNumberCard(Transl.get("My customers"),
                salesDashboardComponentOperation.getCustomerCountAction(),
                SubjectView.ROUTE.concat("/&userId=").concat(String.valueOf(SecurityUtils.getCurrentUserId()))
                        .concat("&onlyCustomer=true")));

        flexCountLayout.add(new AppNumberCard(Transl.get("My opportunities"),
                salesDashboardComponentOperation.getOpportunityCountAction(from, to),
                managementComponentOperation.getOpportunityRoute(from, to, true)));

        flexCountLayout.add(new AppNumberCard(Transl.get("My Offer"),
                salesDashboardComponentOperation.getOfferCountAction(from, to),
                managementComponentOperation.getOfferRoute(from, to, true)));

        flexCountLayout.add(new AppNumberCard(Transl.get("My contracts"),
                salesDashboardComponentOperation.getContractCountAction(from, to),
                managementComponentOperation.getContractRoute(from, to, true)));

        flexCountLayout.add(new AppNumberCard(Transl.get("My invoices"),
                salesDashboardComponentOperation.getInvoiceCountAction(from, to),
                managementComponentOperation.getInvoiceRoute(from, to, true)));

        flexCountLayout.add(new AppNumberCard(Transl.get("My projects"),
                salesDashboardComponentOperation.getProjectCountAction(),
                ProjectView.ROUTE + "/&state=" + ProjectState.REALIZE + "&onlyEditPermission=true"));

        flexCountLayout.add(new AppNumberCard(Transl.get("My events"),
                salesDashboardComponentOperation.getEventCountAction(from, to),
                salesDashboardComponentOperation.getEventRoute(from, to)));

    }

    private void addAppBarCards(CenteredHorizontalLayout barLayout, DatePicker from, DatePicker to) {
        barLayout.add(new AppBarGraphCard(Transl.get("Opportunity states"), null,
                salesDashboardComponentOperation.getOpportunityMapAction(from, to)));

        barLayout.add(new AppBarGraphCard(Transl.get("Offer states"), null,
                salesDashboardComponentOperation.getOfferMapAction(from, to)));
    }

}
