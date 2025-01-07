package cz.bbn.cerberus.dashboard;

import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.orderedlayout.FlexLayout;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDashboardCard;
import cz.bbn.cerberus.commons.component.ui.interfaces.CountActionInt;
import cz.bbn.cerberus.commons.component.ui.interfaces.MapAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.RouteAction;
import cz.bbn.cerberus.commons.component.ui.layouts.CenteredHorizontalLayout;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.invoice.InvoicingService;
import cz.bbn.cerberus.offer.OfferService;
import cz.bbn.cerberus.opportunity.OpportunityService;
import cz.bbn.cerberus.project.ProjectService;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.task.TaskService;
import cz.bbn.cerberus.task.ui.TaskView;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.LocalTime;

@Component
public class SalesDashboardComponentOperation {

    private final SubjectService subjectService;
    private final TaskService taskService;
    private final ContractService contractService;
    private final ProjectService projectService;
    private final InvoicingService invoicingService;
    private final OfferService offerService;
    private final OpportunityService opportunityService;

    public SalesDashboardComponentOperation(SubjectService subjectService, TaskService taskService,
                                            ContractService contractService, ProjectService projectService,
                                            InvoicingService invoicingService, OfferService offerService,
                                            OpportunityService opportunityService) {
        this.subjectService = subjectService;
        this.taskService = taskService;
        this.contractService = contractService;
        this.projectService = projectService;
        this.invoicingService = invoicingService;
        this.offerService = offerService;
        this.opportunityService = opportunityService;
    }

    public void reloadData(FlexLayout countFlexLayout, CenteredHorizontalLayout barLayout) {
        countFlexLayout.getChildren().forEach(child -> ((AppDashboardCard) child).reloadData());

        barLayout.getChildren().forEach(child -> ((AppDashboardCard) child).reloadData());
    }

    public CountActionInt getCustomerCountAction() {
        return () -> subjectService.getCountByUser(SecurityUtils.getCurrentUserId());
    }

    public CountActionInt getOpportunityCountAction(DatePicker from, DatePicker to) {
        return () ->
                opportunityService.getOpportunityCountForDashboard(from.getValue(), to.getValue());
    }

    public CountActionInt getOfferCountAction(DatePicker from, DatePicker to) {
        return () ->
                offerService.getOfferCountForDashboard(from.getValue(), to.getValue());
    }

    public CountActionInt getContractCountAction(DatePicker from, DatePicker to) {
        return () ->
                contractService.getContractCountForDashboard(from.getValue(), to.getValue());
    }

    public CountActionInt getInvoiceCountAction(DatePicker from, DatePicker to) {
        return () ->
                invoicingService.getInvoiceCountForDashboard(from.getValue(), to.getValue());
    }

    public CountActionInt getProjectCountAction() {
        return projectService::getProjectCountForDashboard;
    }

    public MapAction<Double> getOpportunityMapAction(DatePicker from, DatePicker to) {
        return () -> opportunityService.getOpportunityMapForDashboard(from.getValue(), to.getValue());
    }

    public MapAction<Double> getOfferMapAction(DatePicker from, DatePicker to) {
        return () -> offerService.getOfferMapForDashboard(from.getValue(), to.getValue());
    }

    public CountActionInt getEventCountAction(DatePicker from, DatePicker to) {
        return () -> {
            LocalDateTime localDateTimeFrom = from.getValue().atStartOfDay();
            LocalDateTime localDateTimeTo = to.getValue().atTime(LocalTime.MAX);
            return taskService.getCountUserTasks(SecurityUtils.getCurrentUserId(), localDateTimeFrom, localDateTimeTo);
        };
    }

    public RouteAction getEventRoute(DatePicker from, DatePicker to) {
        return () ->
                TaskView.ROUTE
                        + "/&dateFrom=" + from.getValue().toString()
                        + "&dateTo=" + to.getValue().toString()
                        + "&showOnlyMine=true";
    }
}
