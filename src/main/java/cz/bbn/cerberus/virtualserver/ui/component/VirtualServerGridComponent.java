package cz.bbn.cerberus.virtualserver.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.GridSortOrder;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.SortDirection;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.ConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.virtualserver.VirtualServerComponentOperation;
import cz.bbn.cerberus.virtualserver.dto.HddDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerStatus;

import java.util.ArrayList;
import java.util.List;

public class VirtualServerGridComponent extends AppInfiniteGrid<VirtualServerDto> {

    private final VirtualServerComponentOperation componentOperation;

    public VirtualServerGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                      ItemsAction<VirtualServerDto> itemsAction,
                                      VirtualServerComponentOperation componentOperation) {
        super(deleteAction, appEnv, itemsAction);
        this.componentOperation = componentOperation;
        initGrid();
    }

    private void initGrid() {
        addColumn(VirtualServerDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(VirtualServerDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(VirtualServerDto::getOs).setHeader(Transl.get("OS")).setSortable(true).setKey("os");
        addColumn(new ComponentRenderer<>(this::getCpuCores)).setHeader(Transl.get("CPU/cores"))
                .setSortable(true).setKey("cpu");
        addColumn(new ComponentRenderer<>(this::getRam)).setHeader(Transl.get("RAM")).setSortable(true).setKey("ram");
        addColumn(new ComponentRenderer<>(this::getHdd)).setHeader(Transl.get("HDD"));
        addColumn(VirtualServerDto::getIp).setHeader(Transl.get("IP")).setSortable(true).setKey("ip");
        addColumn(new ComponentRenderer<>(this::getUserName)).setHeader(Transl.get("Owner"))
                .setSortable(true).setKey("owner.acronym");
        addColumn(dto -> AppUtils.formatDateTime(dto.getCreationDate(), true))
                .setHeader(Transl.get("Created")).setSortable(true).setKey("creationDate").setFlexGrow(2);
        Column<VirtualServerDto> requestDate =
                addColumn(dto -> AppUtils.formatDateTime(dto.getRequestDate(), true))
                        .setHeader(Transl.get("Requested")).setSortable(true).setKey("requestDate").setFlexGrow(2);
        addColumn(new ComponentRenderer<>(this::getState)).setHeader(Transl.get("Status"))
                .setSortable(true).setKey("status");
        if (componentOperation.getIsInfrastructure()) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_SMALL)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);
        }

        GridSortOrder<VirtualServerDto> sortOrder = new GridSortOrder<>(requestDate, SortDirection.DESCENDING);
        List<GridSortOrder<VirtualServerDto>> sortList = new ArrayList<>();
        sortList.add(sortOrder);
        sort(sortList);

        addItemDoubleClickListener(componentOperation.getEditVirtualServerEvent(this));
    }

    private Span getCpuCores(VirtualServerDto dto) {
        if (dto.getCpu() != null) {
            if (dto.getCores() != null) {
                return new Span(dto.getCpu() + "/" + dto.getCores());
            }
            return new Span(dto.getCpu().toString());
        }
        return new Span();
    }

    private Span getRam(VirtualServerDto dto) {
        if (dto.getRam() != null) {
            return new Span(dto.getRam() + " GB");
        }
        return new Span();
    }

    private VerticalLayout getHdd(VirtualServerDto dto) {
        VerticalLayout layout = new VerticalLayout();
        layout.setMargin(false);
        layout.setPadding(false);
        if (dto.getHddDtoList() != null && !dto.getHddDtoList().isEmpty()) {
            for (HddDto hddDto : dto.getHddDtoList()) {
                layout.add(new Span(hddDto.getName() + ": " + hddDto.getSize() + " GB"));
            }
        }
        return layout;
    }

    private Span getUserName(VirtualServerDto dto) {
        if (dto.getOwner() != null) {
            if (dto.getOwner().getAcronym() != null && !dto.getOwner().getAcronym().isEmpty()) {
                return new Span(dto.getOwner().getAcronym());
            } else {
                return new Span(dto.getOwner().getName());
            }
        }
        return new Span();
    }

    private Span getState(VirtualServerDto dto) {
        if (dto.getStatus() != null) {
            Span span = new Span(Transl.get(dto.getStatus().getValue()));
            span.getElement().getStyle().set("color", dto.getStatus().getColor());
            return span;
        }
        return new Span();
    }

    private HorizontalLayout getGridButtons(VirtualServerDto dto) {
        HorizontalLayout layout = new HorizontalLayout();
        layout.setHeight("44px");
        if (dto.getStatus() == VirtualServerStatus.DELETING) {
            Button delete = VaadinComponents.getDeleteButton();
            delete.addClickListener(e -> {
                ConfirmDialog confirmDialog = new ConfirmDialog(
                        Transl.get("Delete virtual server {0}?", dto.getName()),
                        componentOperation.getDeleteConfirmAction(dto, this));
                confirmDialog.open();
            });
            layout.add(delete);
        }
        return layout;
    }
}
