package cz.bbn.cerberus.contract.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.area.AreaService;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.ui.component.ContractBackOfficeFilterDtoComponent;
import cz.bbn.cerberus.contract.ui.component.ContractGridComponent;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.technology.TechnologyService;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@Route(value = ContractBackOfficeView.ROUTE, layout = MainLayout.class)
@Authorize({Permission.BO_CONTRACT_VIEW, Permission.BO_CONTRACT_LIST_VIEW})
@Slf4j
public class ContractBackOfficeView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "bo-contract-list";

    private final AppEnv appEnv;
    private final UserService userService;
    private final ContractComponentOperation contractComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final ListService listService;
    private final AreaService areaService;
    private final TechnologyService technologyService;

    public ContractBackOfficeView(AppEnv appEnv, UserService userService,
                                  ContractComponentOperation contractComponentOperation,
                                  EntityNewComponentOperation entityNewComponentOperation,
                                  ListService listService, AreaService areaService,
                                  TechnologyService technologyService) {
        this.appEnv = appEnv;
        this.userService = userService;
        this.contractComponentOperation = contractComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.listService = listService;
        this.areaService = areaService;
        this.technologyService = technologyService;
    }

    private void initView(String params) {
        removeAll();

        Button search = VaadinComponents.getSearchButton();
        ContractBackOfficeFilterDtoComponent contractFilterComponent =
                new ContractBackOfficeFilterDtoComponent(search, contractComponentOperation.getOurCompCustomerList(),
                        listService.getEnumerationDtoList("CONTRACT_STATE"),
                        filterBoContractList(listService.getContractTypeDtoList()),
                        contractComponentOperation.getSupplierOwnCompList(),
                        userService.findUserList(), areaService.findAllNotDeletedAreaDtoList(),
                        technologyService.findAllowedTechnologyDtoList(), appEnv, params, getHistoryBreadcrumbs(),
                        ContractInternalType.OPERATIONAL);

        ContractGridComponent grid = new ContractGridComponent(
                contractComponentOperation.getDeleteAction(), appEnv,
                contractComponentOperation.getItemsAction(contractFilterComponent), ContractInternalType.OPERATIONAL);

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Contract list"),
                Permission.CONTRACT_EDIT,
                Transl.get("Add contract"),
                contractComponentOperation.getNewContractDialogEvent(grid, ContractInternalType.OPERATIONAL),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.setId(RobotFrameworkVariables.CONTRACT_VIEW_CARD_ID.getValue());
        card.add(contractFilterComponent);
        card.add(grid);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.BACKOFFICE));
        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            contractFilterComponent.fillUrl();
        });
    }


    private List<ContractTypeDto> filterBoContractList(List<ContractTypeDto> unfilteredList) {
        List<ContractTypeDto> contractTypeList = new ArrayList<>();
        for (ContractTypeDto contractTypeDto : unfilteredList) {
            if (Boolean.TRUE.equals(contractTypeDto.getOperational())) {
                contractTypeList.add(contractTypeDto);
            }
        }
        return contractTypeList;
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
