package cz.bbn.cerberus.contract.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.GridSortOrder;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.provider.SortDirection;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.ui.ContractBackOfficeDetailView;
import cz.bbn.cerberus.contract.ui.ContractSalesDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.List;

public class ContractGridComponent extends AppInfiniteGrid<ContractDto> {

    private final boolean showActions;
    private final ContractInternalType internalType;
    private final AppEnv appEnv;

    public ContractGridComponent(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<ContractDto> itemsAction,
                                 ContractInternalType internalType) {
        super(deleteAction, appEnv, itemsAction);
        showActions = true;
        this.internalType = internalType;
        this.appEnv = appEnv;
        initGrid();
    }

    public ContractGridComponent(ItemsAction<ContractDto> itemsAction, AppEnv appEnv) {
        super(appEnv, itemsAction);
        showActions = false;
        this.appEnv = appEnv;
        this.internalType = ContractInternalType.SALES;
        initGrid();
    }

    private void initGrid() {
        addColumn(ContractDto::getId).setHeader(Transl.get("Contract number")).setSortable(true)
                .setKey("id");
        addColumn(contractDto -> AppUtils.formatDate(contractDto.getValidityStart()))
                .setHeader(Transl.get("Validity start"))
                .setSortable(true)
                .setKey("validityStart");
        if (internalType == ContractInternalType.SALES) {
            addColumn(new ComponentRenderer<>(this::getCustomerColumn)).setHeader(Transl.get("Customer"))
                    .setSortable(true).setKey("subjectDto.id");
        }
        if (internalType == ContractInternalType.OPERATIONAL) {
            addColumn(new ComponentRenderer<>(this::getContractPartyColumn)).setHeader(Transl.get("Contract party"))
                    .setSortable(true).setKey("contractParty");
        }
        addColumn(ContractDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(this::getUserName)).setKey("userEntity.name")
                .setHeader(Transl.get("Owner")).setSortable(true);
        addColumn(new ComponentRenderer<>(this::getContractStateColumn)).setHeader(Transl.get("Contract state"))
                .setSortable(true).setKey("contractState.name");
        addColumn(new ComponentRenderer<>(this::getType)).setHeader(Transl.get("Internal type"));
        Column<ContractDto> lastUpdate = addColumn(ContractDto::getLastUpdate).setKey("lastUpdate");
        lastUpdate.setVisible(false);
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        GridSortOrder<ContractDto> sortOrder = new GridSortOrder<>(lastUpdate, SortDirection.DESCENDING);
        List<GridSortOrder<ContractDto>> sortList = new ArrayList<>();
        sortList.add(sortOrder);
        sort(sortList);

        if (showActions) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);
        }

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));

    }

    private Span getCustomerColumn(ContractDto contractDto) {
        Span span = new Span();
        if (contractDto.getSubjectDto() != null) {
            span.add(contractDto.getSubjectDto().getId());
            span.getElement().setProperty(TextValues.TITLE, contractDto.getSubjectDto().getName());
        }
        return span;
    }

    private Span getContractStateColumn(ContractDto contractDto) {
        Span span = new Span();
        if (contractDto.getContractState() != null) {
            span.add(contractDto.getContractState().getName());
            span.getElement().setProperty(TextValues.TITLE, contractDto.getContractState().getName());
        }
        return span;
    }

    private Span getContractPartyColumn(ContractDto contractDto) {
        Span span = new Span();
        if (contractDto.getContractParty() != null) {
            span.add(contractDto.getContractParty().getName());
            span.getElement().setProperty(TextValues.TITLE, contractDto.getContractParty().getName());
        }
        return span;
    }

    private Span getType(ContractDto contractDto) {
        Span span = new Span();
        if (contractDto.getInternalType() == ContractInternalType.SALES) {
            span.add(VaadinIcon.GOLF.create());
        }
        if (contractDto.getInternalType() == ContractInternalType.SUPPLIER) {
            span.add(VaadinIcon.PACKAGE.create());
        }
        return span;
    }

    private HorizontalLayout getGridButtons(ContractDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit contract",
                "Are you sure you want to delete contract {0} ?", "Delete contract");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.CONTRACT_VIEW, Permission.CONTRACT_DELETE, clickedItem.getId(), clickedItem.getName(),
                ContractSalesDetailView.ROUTE, DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), clickedItem.getDeleted());
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }

    private void gridClicked(String code) {
        if (SecurityUtils.hasCustomPermission(DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), code,
                Permission.CONTRACT_VIEW.name())) {
            if (ContractInternalType.SALES == internalType) {
                UI.getCurrent().navigate(ContractSalesDetailView.ROUTE + "/" + code);
            }
            if (ContractInternalType.OPERATIONAL == internalType) {
                UI.getCurrent().navigate(ContractBackOfficeDetailView.ROUTE + "/" + code);
            }
        } else {
            ErrorNotification.show(Transl.get(ErrorCode.VIEW_PERMISSION_MISSING.getError()), appEnv);
        }
    }

    private Span getUserName(ContractDto dto) {
        if (dto.getUserDto() != null) {
            if (dto.getUserDto().getAcronym() != null && !dto.getUserDto().getAcronym().isEmpty()) {
                return new Span(dto.getUserDto().getAcronym());
            } else {
                return new Span(dto.getUserDto().getName());
            }
        }
        return new Span();
    }
}
