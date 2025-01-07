package cz.bbn.cerberus.offer.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;

import java.util.List;
import java.util.stream.Collectors;

public class OfferNewDialog extends AppDialog {

    private final AppInfiniteGrid<?> grid;
    private final OfferComponentOpperation offerComponentOpperation;
    private final OpportunityDto opportunityDto;
    private final UserService userService;
    private final ListService listService;
    private final SubjectDto subjectDto;
    private final AppEnv appEnv;
    private final SubjectService subjectService;

    public OfferNewDialog(AppInfiniteGrid<?> grid, OfferComponentOpperation offerComponentOpperation,
                          OpportunityDto opportunityDto, UserService userService, ListService listService,
                          AppEnv appEnv, SubjectService subjectService) {
        this.grid = grid;
        this.offerComponentOpperation = offerComponentOpperation;
        this.opportunityDto = opportunityDto;
        this.userService = userService;
        this.listService = listService;
        this.appEnv = appEnv;
        this.subjectDto = null;
        this.subjectService = subjectService;
        initComponent();
    }

    public OfferNewDialog(AppInfiniteGrid<?> grid, OfferComponentOpperation offerComponentOpperation,
                          SubjectDto subjectDto, UserService userService, ListService listService, AppEnv appEnv,
                          OpportunityDto opportunityDto, SubjectService subjectService) {
        this.grid = grid;
        this.offerComponentOpperation = offerComponentOpperation;
        this.appEnv = appEnv;
        this.opportunityDto = opportunityDto;
        this.userService = userService;
        this.listService = listService;
        this.subjectDto = subjectDto;
        this.subjectService = subjectService;
        initComponent();
    }

    private void initComponent() {
        setTitle(Transl.get("Add Offer"));

        OfferDto dto = new OfferDto();
        dto.setDeleted(Boolean.FALSE);
        dto.setOpportunityDto(opportunityDto);
        dto.setProcessedByUserDto(SecurityUtils.getCurrentUserDto());
        List<SubjectDto> ownOrganizationSubjectList = listService.getSubjectDtoListByOwnCompany();
        SubjectDto defaultOwnCompany = ownOrganizationSubjectList.stream()
                .filter(actualSubjectDto -> actualSubjectDto.getId().equals(appEnv.getDefaultCompanySubject()))
                .findAny().orElse(null);
        dto.setOwnOrganizationSubjectDto(defaultOwnCompany);
        List<OpportunityDto> opportunityDtoList = subjectDto == null ?
                listService.getOpportunityDtoList().stream().filter(actualOpportunityDto ->
                        !Boolean.TRUE.equals(actualOpportunityDto.getDeleted())).collect(Collectors.toList())
                : offerComponentOpperation.findBySubjectId(subjectDto.getId());

        Binder<OfferDto> binder = new Binder<>();
        if (opportunityDto != null) {
            dto.setSubjectDto(opportunityDto.getSubject());
            OfferDetailComponent offerDetailComponent = new OfferDetailComponent(dto, binder,
                    opportunityDtoList, userService.findUserList(), ownOrganizationSubjectList, null, false,
                    subjectService.getAllowedCustomers());
            setContent(offerDetailComponent);
        } else {
            dto.setSubjectDto(subjectDto);
            OfferDetailComponent offerDetailComponent = new OfferDetailComponent(dto, binder,
                    opportunityDtoList, userService.findUserList(),
                    ownOrganizationSubjectList, false, subjectService.getAllowedCustomers());
            setContent(offerDetailComponent);
        }


        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                offerComponentOpperation.getSaveAction(this).saveItem(dto, new OfferDto());
                if (grid != null) {
                    grid.loadData();
                }
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }
}
