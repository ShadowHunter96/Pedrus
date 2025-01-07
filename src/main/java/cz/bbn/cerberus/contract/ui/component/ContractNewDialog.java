package cz.bbn.cerberus.contract.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractEndingDays;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.ui.component.tab.ContractDetailTab;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;

import java.util.List;


public class ContractNewDialog extends AppDialog {

    private final AppInfiniteGrid<ContractDto> grid;
    private final ContractComponentOperation contractComponentOperation;
    private final UserService userService;
    private final SubjectDto subjectDto;
    private final List<ContractTypeDto> contractTypeDtoList;
    private final AppEnv appEnv;
    private final ListService listService;
    private final ContractInternalType contractInternalType;
    private final OpportunityDto opportunityDto;

    public ContractNewDialog(AppInfiniteGrid<ContractDto> grid, ContractComponentOperation contractComponentOperation,
                             UserService userService,
                             SubjectDto subjectDto,
                             List<ContractTypeDto> contractTypeDtoList, AppEnv appEnv, ListService listService,
                             ContractInternalType contractInternalType, OpportunityDto opportunityDto) {
        this.grid = grid;
        this.contractComponentOperation = contractComponentOperation;
        this.userService = userService;
        this.subjectDto = subjectDto;
        this.contractTypeDtoList = contractTypeDtoList;
        this.appEnv = appEnv;
        this.listService = listService;
        this.contractInternalType = contractInternalType;
        this.opportunityDto = opportunityDto;
        init();
    }

    void init() {
        setTitle(Transl.get("New contract"));

        ContractDto dto = new ContractDto();
        dto.setUserDto(SecurityUtils.getCurrentUserDto());
        dto.setSendNotificationDaysBefore(ContractEndingDays.DAYS_30);
        SubjectDto defaultOwnCompany = listService.getSubjectDtoListByOwnCompany().stream()
                .filter(actualSubjectDto -> actualSubjectDto.getId().equals(appEnv.getDefaultCompanySubject()))
                .findAny().orElse(null);
        if (contractInternalType == ContractInternalType.SALES) {
            dto.setContractParty(defaultOwnCompany);
        }
        if (subjectDto != null && !subjectDto.getOwnCompany()) {
            dto.setSubjectDto(subjectDto);
        }
        if (opportunityDto != null) {
            dto.setOpportunityDto(opportunityDto);
        }
        dto.setInternalType(contractInternalType);

        ComboBox<ContractDto> connectedContract = new ComboBox<>();

        ContractDetailTab contractDetailTab =
                new ContractDetailTab(dto, contractComponentOperation.getSaveAction(null, this,
                        connectedContract), appEnv, contractComponentOperation.findContractAllowedList(),
                        userService.findUserList(), true, false,
                        contractTypeDtoList, null, listService,
                        connectedContract, contractComponentOperation);
        setContent(contractDetailTab);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            contractDetailTab.saveItem();
            if (grid != null) {
                grid.loadData();
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }
}
