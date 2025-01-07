package cz.bbn.cerberus.management;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.orderedlayout.FlexLayout;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDashboardCard;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.CountActionInt;
import cz.bbn.cerberus.commons.component.ui.interfaces.MapItemDtoAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.RouteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.StringAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.contract.ui.ContractSalesView;
import cz.bbn.cerberus.employee.EmployeeService;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.invoice.InvoicingService;
import cz.bbn.cerberus.invoice.ui.InvoicingView;
import cz.bbn.cerberus.offer.OfferService;
import cz.bbn.cerberus.offer.ui.OfferView;
import cz.bbn.cerberus.opportunity.OpportunityService;
import cz.bbn.cerberus.opportunity.dto.OpportunityState;
import cz.bbn.cerberus.opportunity.ui.OpportunityView;
import cz.bbn.cerberus.permissionmanagement.dto.OwnerEntityDto;
import cz.bbn.cerberus.project.ProjectService;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.workreport.WorkReportService;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
public class ManagementComponentOperation {

    private final SubjectService subjectService;
    private final OpportunityService opportunityService;
    private final OfferService offerService;
    private final ContractService contractService;
    private final InvoicingService invoicingService;
    private final ProjectService projectService;
    private final EmployeeService employeeService;
    private final WorkReportService workReportService;
    private final HolidayService holidayService;
    private final OwnerService ownerService;
    private final AppEnv appEnv;

    public ManagementComponentOperation(SubjectService subjectService, OpportunityService opportunityService,
                                        EmployeeService employeeService, WorkReportService workReportService,
                                        HolidayService holidayService, OfferService offerService,
                                        ContractService contractService, InvoicingService invoicingService,
                                        ProjectService projectService, OwnerService ownerService, AppEnv appEnv) {
        this.subjectService = subjectService;
        this.opportunityService = opportunityService;
        this.offerService = offerService;
        this.contractService = contractService;
        this.invoicingService = invoicingService;
        this.projectService = projectService;
        this.employeeService = employeeService;
        this.workReportService = workReportService;
        this.holidayService = holidayService;
        this.ownerService = ownerService;
        this.appEnv = appEnv;
    }

    public void reloadData(FlexLayout circleGraphsFlexLayout) {
        circleGraphsFlexLayout.getChildren().forEach(child -> ((AppDashboardCard) child).reloadData());
    }

    public MapItemDtoAction<ItemDto, Double> getCustomerByUserMapAction(List<UserDto> userDtoList) {
        return () -> subjectService.getCustomersMapByUser(userDtoList);
    }

    public MapItemDtoAction<ItemDto, Double> getOpportunitiesByUserMapAction(List<UserDto> userDtoList,
                                                                             DatePicker from, DatePicker to) {
        return () -> opportunityService.getOpportunitiesMapByUser(userDtoList, from.getValue(), to.getValue());
    }

    public MapItemDtoAction<ItemDto, Double> getOffersByUserMapAction(List<UserDto> userDtoList, DatePicker from,
                                                                      DatePicker to) {
        return () -> offerService.getOffersMapByUser(userDtoList, from.getValue(), to.getValue());
    }

    public MapItemDtoAction<ItemDto, Double> getContractsByUserMapAction(List<UserDto> userDtoList, DatePicker
            from, DatePicker to) {
        return () -> contractService.getContractsMapByUser(userDtoList, from.getValue(), to.getValue());
    }

    public MapItemDtoAction<ItemDto, Double> getInvoicesByUserMapAction(List<UserDto> userDtoList, DatePicker
            from, DatePicker to) {
        return () -> invoicingService.getInvoicesMapByUser(userDtoList, from.getValue(), to.getValue());
    }

    public MapItemDtoAction<ItemDto, Double> getProjectsByUserMapAction(List<UserDto> userDtoList) {
        return () -> projectService.getProjectsMapByUser(userDtoList);
    }

    public CountActionInt getEmployeeCount(DatePicker from, DatePicker to) {
        return () -> employeeService.getEmployeeCountFromTo(from, to);
    }

    public StringAction getReportedHours(DatePicker from, DatePicker to) {
        return () -> workReportService.getActiveWorkReport(from, to, employeeService.getEmployeeIdSetFromTo(from, to));
    }

    public CountActionInt getRequiredHours(DatePicker from, DatePicker to) {
        return () -> getRequiredHoursTotal(from, to);
    }

    public RouteAction getContractRoute(DatePicker from, DatePicker to, boolean onlyEditPermission) {
        return () ->
                ContractSalesView.ROUTE
                        + "/&validityStartFrom=" + from.getValue().toString()
                        + "&validityStartTo=" + to.getValue().toString()
                        + (onlyEditPermission ? "&onlyEditPermission=true" : "");
    }

    public RouteAction getOfferRoute(DatePicker from, DatePicker to, boolean onlyEditPermission) {
        return () ->
                OfferView.ROUTE
                        + "/&validityDateFrom=" + from.getValue().toString()
                        + "&validityDateTo=" + to.getValue().toString()
                        + (onlyEditPermission ? "&onlyEditPermission=true" : "");
    }

    public RouteAction getOpportunityRoute(DatePicker from, DatePicker to, boolean onlyEditPermission) {
        return () ->
                OpportunityView.ROUTE
                        + "/&startDateFrom=" + from.getValue().toString()
                        + "&startDateTo=" + to.getValue().toString()
                        + "&state=" + OpportunityState.NOT_SUBMITED + "," + OpportunityState.IN_PROGRESS + ","
                        + OpportunityState.SUBMITTED + "," + OpportunityState.WON
                        + (onlyEditPermission ? "&onlyEditPermission=true" : "");
    }

    public RouteAction getInvoiceRoute(DatePicker from, DatePicker to, boolean onlyEditPermission) {
        return () ->
                InvoicingView.ROUTE
                        + "/&invoicingDateStart=" + from.getValue().toString()
                        + "&invoicingDateEnd=" + to.getValue().toString()
                        + (onlyEditPermission ? "&onlyEditPermission=true" : "");
    }

    public SaveAction<List<OwnerEntityDto>> getSaveAction(ComboBox<ObjectType> objectType, MultiSelectComboBox<UserDto> user){
        return (list, originalDto) -> {
            ownerService.saveOwnerEntityList(list, objectType.getValue(), user.getValue());
            SuccessNotification.showSavingSuccess(appEnv);
        };
    }

    public void bulkSave(Set<ObjectType> objectTypeSet, UserDto owner, UserDto newOwner){
        ownerService.bulkSave(objectTypeSet, owner, newOwner);
    }

    private Long getRequiredHoursTotal(DatePicker from, DatePicker to) {
        long required = 0L;
        Set<LocalDate> holidayList = getHolidays();
        LocalDate start = AppUtils.getMonthStart(from.getValue());
        int i = 0;
        while (true) {
            LocalDate currentDate = start.plusDays(i);
            if (currentDate.getMonth() == to.getValue().plusMonths(1).getMonth()) {
                break;
            }
            i++;
            if (!holidayList.contains(currentDate) && currentDate.getDayOfWeek().getValue() != 6
                    && currentDate.getDayOfWeek().getValue() != 7) {
                required = required + 8;
            }
        }
        return required * getEmployeeCount(from, to).getCount();
    }

    private Set<LocalDate> getHolidays() {
        Set<LocalDate> holidayList = new HashSet<>();
        for (HolidayEntity holiday : holidayService.findAll()) {
            holidayList.add(holiday.getDate());
        }
        return holidayList;
    }
}
