package cz.bbn.cerberus.subject.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractFilterDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.ui.component.ContractGridComponent;
import cz.bbn.cerberus.subject.dto.SubjectDto;

public class SubjectContractTab extends TabSimpleComponent {

    private final ContractComponentOperation contractComponentOperation;
    private final AppEnv appEnv;
    private final SubjectDto subjectDto;
    private ContractGridComponent grid;

    public SubjectContractTab(ContractComponentOperation contractComponentOperation,
                              AppEnv appEnv, SubjectDto subjectDto) {
        this.contractComponentOperation = contractComponentOperation;
        this.appEnv = appEnv;
        this.subjectDto = subjectDto;
        initTab();
    }

    private void initTab() {
        ContractFilterDto contractFilterDto = new ContractFilterDto();
        contractFilterDto.setCustomerDto(subjectDto);
        grid = new ContractGridComponent(
                contractComponentOperation.getDeleteAction(), appEnv,
                contractComponentOperation.getItemsAction(contractFilterDto), ContractInternalType.SALES);
        this.add(grid);
        this.setSizeFull();
    }

    @Override
    public void loadTab() {
        grid.loadData();
    }

    public AppInfiniteGrid<ContractDto> getGrid() {
        return grid;
    }
}
