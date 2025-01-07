package cz.bbn.cerberus.project.ui.component.tab;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.email.ui.component.EmailFilterComponent;
import cz.bbn.cerberus.email.ui.component.EmailGridComponent;
import cz.bbn.cerberus.project.dto.ProjectDto;

public class ProjectEmailTab extends TabSimpleComponent {

    private final ProjectDto projectDto;
    private final EmailComponentOperations emailComponentOperations;
    private final AppEnv appEnv;

    private EmailGridComponent grid;

    public ProjectEmailTab(ProjectDto projectDto, EmailComponentOperations emailComponentOperations, AppEnv appEnv) {
        this.projectDto = projectDto;
        this.emailComponentOperations = emailComponentOperations;
        this.appEnv = appEnv;
        initTab();
    }

    private void initTab() {
        Button search = VaadinComponents.getSearchButton();
        EmailFilterComponent filterComponent = new EmailFilterComponent(search, emailComponentOperations,
                DomainEnum.PROJECT_DOMAIN_NAME, projectDto.getId());

        grid = new EmailGridComponent(emailComponentOperations.getDeleteAction(), appEnv,
                emailComponentOperations.getItemsAction(filterComponent), emailComponentOperations);

        search.addClickListener(e -> grid.loadData());
        this.add(filterComponent, grid);
        this.setSizeFull();
    }

    @Override
    public void loadTab() {
        grid.loadData();
    }
}
