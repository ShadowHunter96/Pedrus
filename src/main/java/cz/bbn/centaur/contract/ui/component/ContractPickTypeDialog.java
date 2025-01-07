package cz.bbn.cerberus.contract.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

public class ContractPickTypeDialog extends AppDialog {

    private final AppInfiniteGrid<ContractDto> grid;
    private final ContractComponentOperation componentOperation;
    private final SubjectDto subjectDto;
    private final OpportunityDto opportunityDto;

    public ContractPickTypeDialog(AppInfiniteGrid<ContractDto> grid, ContractComponentOperation componentOperation,
                                  SubjectDto subjectDto, OpportunityDto opportunityDto) {
        this.grid = grid;
        this.componentOperation = componentOperation;
        this.subjectDto = subjectDto;
        this.opportunityDto = opportunityDto;
        init();
    }

    private void init() {
        setTitle(Transl.get("Sales or supplier contract"));
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        Icon salesIcon = VaadinIcon.GOLF.create();
        salesIcon.setSize("2.5em");
        Icon supplierIcon = VaadinIcon.PACKAGE.create();
        supplierIcon.setSize("2.5em");
        Button sales = VaadinComponents.getButton(Transl.get("Sales"), salesIcon);
        Button supplier = VaadinComponents.getButton(Transl.get("Supplier co."), supplierIcon);
        sales.setWidth("13em");
        sales.setHeight("5em");
        supplier.setWidth("13em");
        supplier.setHeight("5em");
        horizontalLayout.add(sales, supplier);
        setContent(horizontalLayout);
        addCloseButton();

        sales.setDisableOnClick(true);
        sales.addClickListener(componentOperation.getNewContractDialogEvent(
                grid, subjectDto, ContractInternalType.SALES, this, opportunityDto));

        supplier.setDisableOnClick(true);
        supplier.addClickListener(componentOperation.getNewContractDialogEvent(
                grid, subjectDto, ContractInternalType.SUPPLIER, this, opportunityDto));
    }
}
