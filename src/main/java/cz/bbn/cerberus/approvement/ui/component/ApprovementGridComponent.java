package cz.bbn.cerberus.approvement.ui.component;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.grid.GridSortOrder;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.data.provider.SortDirection;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.approvement.ApprovementComponentOperation;
import cz.bbn.cerberus.approvement.dto.ApprovementDto;
import cz.bbn.cerberus.approvement.dto.ApprovementSimpleDto;
import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.CountActionDouble;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.time.YearMonth;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class ApprovementGridComponent extends AppInfiniteGrid<ApprovementSimpleDto> {

    private final ApprovementComponentOperation approvementComponentOperation;
    private final ListService listService;
    private final CountActionDouble countActionDouble;
    private final List<RoleDto> roleList;
    private final ApprovementType approvementType;
    private final List<EmployeeDto> employeeDtoList;
    private final EmployeeDto userEmployeeDto;
    private final EnumerationComponentOperation enumerationComponentOperation;
    private final List<HolidayEntity> holidayEntityList;
    private final ComboBox<Integer> year;

    public ApprovementGridComponent(AppEnv appEnv, ItemsAction<ApprovementSimpleDto> itemsAction,
                                    ApprovementComponentOperation approvementComponentOperation,
                                    ListService listService, CountActionDouble countActionDouble,
                                    List<RoleDto> roleList,
                                    ApprovementType approvementType, List<EmployeeDto> employeeDtoList,
                                    EmployeeDto userEmployeeDto,
                                    EnumerationComponentOperation enumerationComponentOperation,
                                    List<HolidayEntity> holidayEntityList, ComboBox<Integer> year) {
        super(appEnv, itemsAction);
        this.approvementComponentOperation = approvementComponentOperation;
        this.listService = listService;
        this.holidayEntityList = holidayEntityList;
        this.countActionDouble = countActionDouble;
        this.roleList = roleList;
        this.approvementType = approvementType;
        this.employeeDtoList = employeeDtoList;
        this.userEmployeeDto = userEmployeeDto;
        this.enumerationComponentOperation = enumerationComponentOperation;
        this.year = year;
        initGrid();
    }

    private void initGrid() {
        addColumn(ApprovementSimpleDto::getId).setWidth("4em").setHeader(Transl.get("Id"))
                .setSortable(true).setKey("id");
        addColumn(dto -> AppUtils.formatDate(dto.getDateFrom())).setWidth("7em").setHeader(Transl.get("From"))
                .setSortable(true).setKey("dateFrom");
        addColumn(dto -> AppUtils.formatDate(dto.getDateTo())).setWidth("7em").setHeader(Transl.get("To"))
                .setSortable(true).setKey("dateTo");
        addColumn(this::getEmployeeName).setSortable(true).setHeader(Transl.get("Employee"))
                .setKey("ownerUserEntity.employee.firstName");
        addColumn(new ComponentRenderer<>(this::getTypeIcons)).setWidth("2em").setHeader(Transl.get("Type"))
                .setSortable(true).setKey("approvementType");
        addColumn(new ComponentRenderer<>(this::getDays)).setSortable(true)
                .setHeader(Transl.get("Work days")).setKey("days");
        if (approvementType != null && approvementType == ApprovementType.BUSSINES_TRIP) {
            addColumn(new ComponentRenderer<>(this::getFellowPassengers)).setHeader(Transl.get("Passengers"));
        }
        addColumn(new ComponentRenderer<>(this::getLMApprovedIcon))
                .setHeader(Transl.get("LM approved")).setSortable(true).setKey("lineManageApproved");
        addColumn(new ComponentRenderer<>(this::getSuperiorApprovedIcon))
                .setHeader(Transl.get("Superior approved")).setSortable(true).setKey("superiorApproved");
        addColumn(new ComponentRenderer<>(this::getState)).setHeader(Transl.get("State"))
                .setSortable(true).setKey("approvementState");
        Column<ApprovementSimpleDto> createdColumn = addColumn(approvementSimpleDto ->
                AppUtils.formatDateTime(approvementSimpleDto.getCreated(), true)
        ).setHeader(Transl.get("Created")).setSortable(true).setKey("created");

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));

        GridSortOrder<ApprovementSimpleDto> sortOrder = new GridSortOrder<>(createdColumn, SortDirection.DESCENDING);
        List<GridSortOrder<ApprovementSimpleDto>> sortList = new ArrayList<>();
        sortList.add(sortOrder);
        sort(sortList);
    }

    private String getEmployeeName(ApprovementSimpleDto dto) {
        return dto.getOwnerUserDto().getEmployee().getFirstName() + " " +
                dto.getOwnerUserDto().getEmployee().getLastName();
    }

    private Span getDays(ApprovementSimpleDto dto) {
        Span span = new Span();
        span.setClassName("centered");
        if (dto.getDateTo().getYear() > year.getValue()) {
            span.add(String.valueOf(AppUtils.countWorkDays(dto.getDateFrom(), YearMonth.now().withYear(
                    dto.getDateFrom().getYear()).withMonth(12).atEndOfMonth(), holidayEntityList)));
        } else if (dto.getDateFrom().getYear() < year.getValue()) {
            span.add(String.valueOf(AppUtils.countWorkDays(YearMonth.now().withYear(
                    dto.getDateTo().getYear()).withMonth(1).atDay(1), dto.getDateTo(), holidayEntityList)));
        } else {
            span.add(dto.getDays().toString());
        }
        return span;
    }

    private Span getFellowPassengers(ApprovementSimpleDto dto) {
        Span span = new Span();
        List<String> fellowPassengerList = dto.getApprovementBusinessTripDto().getFellowPassengers(employeeDtoList)
                .stream().map(obj -> obj.getFirstName().concat(" ").concat(obj.getLastName()))
                .collect(Collectors.toList());
        String passengers = StringUtils.join(fellowPassengerList, ", ");
        span.getElement().setAttribute("title", passengers);
        span.add(passengers);
        return span;
    }

    private void gridClicked(Long id) {
        ApprovementDto approvementDto = approvementComponentOperation.getApprovementDto(id);
        boolean master = SecurityUtils.hasPermission(Permission.APPROVEMENT_EDIT_ALL) &&
                !approvementDto.getOwnerUserDto().getId().equals(approvementDto.getCreatedUserDto().getId());
        ApprovementDialog approvementDialog = new ApprovementDialog(approvementDto.getApprovementType(),
                this, approvementComponentOperation, approvementDto, listService.getAllowedProjectDtoList(),
                listService.getEmployeeDtoList(), holidayEntityList, countActionDouble, roleList,
                listService.getSubjectDtoList(), listService.getOpportunityListReadPermission(), userEmployeeDto,
                enumerationComponentOperation, master);
        approvementDialog.open();
    }

    private Span getTypeIcons(ApprovementSimpleDto clickedItem) {
        Span span = new Span();
        span.setClassName("centered");
        Icon icon = clickedItem.getApprovementType().getIcon().create();
        icon.setClassName("centered-icon");
        icon.getElement().setAttribute("title", Transl.get(clickedItem.getApprovementType().name()));
        span.add(icon);
        return span;
    }

    private Span getLMApprovedIcon(ApprovementSimpleDto clickedItem) {
        Span span = new Span();
        if (clickedItem.getApprovementType() != ApprovementType.ILL) {
            span.setClassName("centered");
            Icon icon = getIconLine(clickedItem);
            if (icon != null) {
                icon.setClassName("centered-icon");
                span.add(icon);
            }
        }
        return span;
    }

    private Span getState(ApprovementSimpleDto clickedItem) {
        Span span = new Span(Transl.get(clickedItem.getApprovementState().name()));
        span.setClassName(clickedItem.getApprovementState().getColorClass());
        span.getElement().setAttribute("title", Transl.get(clickedItem.getApprovementState().name()));
        return span;
    }

    private Span getSuperiorApprovedIcon(ApprovementSimpleDto clickedItem) {
        Span span = new Span();
        if (clickedItem.getApprovementType() != ApprovementType.ILL) {
            span.setClassName("centered");
            Icon icon = getIconSuperior(clickedItem);
            if (icon != null) {
                icon.setClassName("centered-icon");
                span.add(icon);
            }
        }
        return span;
    }

    private Icon getIconLine(ApprovementSimpleDto dto) {
        if ((dto.getLineManagerId() != null || dto.getLineManagerRoleId() != null) && dto.getSuperiorId() != null
                && !Objects.equals(dto.getSuperiorId(), dto.getLineManagerId())) {
            if (dto.getApprovementState() == ApprovementState.WAITING_FOR_SUPERIOR
                    || dto.getApprovementState() == ApprovementState.COMPLETELY_APPROVED
                    || dto.getApprovementState() == ApprovementState.DENIED) {
                return VaadinComponents.getBooleanIcon(dto.getLineManageApproved());
            } else {
                return VaadinIcon.QUESTION.create();
            }
        }
        return null;
    }

    private Icon getIconSuperior(ApprovementSimpleDto dto) {
        if (dto.getSuperiorId() != null || dto.getSuperiorRoleId() != null) {
            if (dto.getApprovementState() == ApprovementState.COMPLETELY_APPROVED
                    || dto.getApprovementState() == ApprovementState.DENIED) {
                return VaadinComponents.getBooleanIcon(dto.getSuperiorApproved());
            } else {
                return VaadinIcon.QUESTION.create();
            }
        }
        return null;
    }

}
