package cz.bbn.cerberus.subject.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.contextmenu.GridContextMenu;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.Map;

public class SubjectGridComponent extends AppInfiniteGrid<SubjectDto> {

    private final boolean showButtons;
    private final Map<String, SupplierTypeDto> supplierTypeMap;
    private final AppEnv appEnv;

    public SubjectGridComponent(DeleteAction deleteAction, ItemsAction<SubjectDto> itemsAction, AppEnv appEnv,
                                Map<String, SupplierTypeDto> supplierTypeMap) {
        super(deleteAction, appEnv, itemsAction);
        this.supplierTypeMap = supplierTypeMap;
        this.showButtons = true;
        this.appEnv = appEnv;
        initGrid();
    }

    public SubjectGridComponent(ItemsAction<SubjectDto> itemsAction, AppEnv appEnv,
                                Map<String, SupplierTypeDto> supplierTypeMap) {
        super(appEnv, itemsAction);
        this.showButtons = false;
        this.supplierTypeMap = supplierTypeMap;
        this.appEnv = appEnv;
        initGrid();
    }

    private void initGrid() {
        setSizeFull();
        addColumn(SubjectDto::getId).setHeader(Transl.get("Subject shortcut")).setSortable(true).setKey("id");
        addColumn(SubjectDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        if (supplierTypeMap != null) {
            addColumn(new ComponentRenderer<>(this::getSupplierType)).setHeader(Transl.get("Supplier type"))
                    .setSortable(true).setKey("supplierType");
        }
        addColumn(SubjectDto::getDescription).setHeader(Transl.get("Description"))
                .setSortable(true).setKey("description");
        addColumn(new ComponentRenderer<>(this::getUserName)).setHeader(Transl.get("Owner"))
                .setSortable(true).setKey("userEntity.acronym");
        addColumn(new ComponentRenderer<>(this::getIcons)).setHeader(Transl.get("Subject type"))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0);

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        if (showButtons) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

            addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
        }

        GridContextMenu<SubjectDto> contextMenu = addContextMenu();
        contextMenu.addItem(Transl.get("Edit"), e -> {
            if (e.getItem().isPresent()) {
                gridClicked(e.getItem().get().getId());
            } else {
                ErrorNotification.show(Transl.get("No item selected"), getAppEnv());
            }
        });
        contextMenu.addItem(Transl.get("Delete"), e -> {
            if (e.getItem().isPresent()) {
                getDeleteAction(getDataVariables(e.getItem().get()), getStringVariables(), this, false);
            } else {
                ErrorNotification.show(Transl.get("No item selected"), getAppEnv());
            }
        });

    }

    private HorizontalLayout getIcons(SubjectDto clickedItem) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        if (clickedItem.getCustomer().booleanValue()) {
            Icon icon = VaadinIcon.GOLF.create();
            icon.getElement().setAttribute("title", Transl.get("Customer"));
            horizontalLayout.add(icon);
        }
        if (clickedItem.getSupplier().booleanValue()) {
            Icon icon = VaadinIcon.PACKAGE.create();
            icon.getElement().setAttribute("title", Transl.get("Supplier"));
            horizontalLayout.add(icon);
        }
        if (clickedItem.getOwnCompany().booleanValue()) {
            Icon icon = VaadinIcon.HOME.create();
            icon.getElement().setAttribute("title", Transl.get("Own company"));
            horizontalLayout.add(icon);
        }
        return horizontalLayout;
    }

    private HorizontalLayout getGridButtons(SubjectDto clickedItem) {
        return getSimpleGridButtons(getDataVariables(clickedItem), getStringVariables(), this);
    }

    private void gridClicked(String code) {
        if (SecurityUtils.hasCustomPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), code,
                Permission.SUBJECT_VIEW.name())) {
            UI.getCurrent().navigate(SubjectDetailView.ROUTE + "/" + code);
        } else {
            ErrorNotification.show(Transl.get(ErrorCode.VIEW_PERMISSION_MISSING.getError()), appEnv);
        }
    }

    private Span getSupplierType(SubjectDto clickedItem) {
        if (clickedItem.getSupplierType() == null
                || !supplierTypeMap.containsKey(clickedItem.getSupplierType().getId())) {
            return new Span("");
        } else {
            return new Span(supplierTypeMap.get(clickedItem.getSupplierType().getId()).getName());
        }

    }

    private AppGridStringVariables getStringVariables() {
        return new AppGridStringVariables("Edit subject",
                "Are you sure you want to delete subject {0} ?", "Delete subject");
    }

    private AppGridDataVariables getDataVariables(SubjectDto clickedItem) {
        return new AppGridDataVariables(
                Permission.SUBJECT_EDIT, Permission.SUBJECT_DELETE, clickedItem.getId(), clickedItem.getName(),
                SubjectDetailView.ROUTE, DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), clickedItem.getDeleted());
    }

    private Span getUserName(SubjectDto dto) {
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
