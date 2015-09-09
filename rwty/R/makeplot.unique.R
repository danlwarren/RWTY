


<!DOCTYPE html>
<html lang="en" class=" is-copy-enabled">
  <head prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# object: http://ogp.me/ns/object# article: http://ogp.me/ns/article# profile: http://ogp.me/ns/profile#">
    <meta charset='utf-8'>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta http-equiv="Content-Language" content="en">
    <meta name="viewport" content="width=1020">
    
    
    <title>RWTY/output_plot.R at bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1 · danlwarren/RWTY</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub">
    <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub">
    <link rel="apple-touch-icon" sizes="57x57" href="/apple-touch-icon-114.png">
    <link rel="apple-touch-icon" sizes="114x114" href="/apple-touch-icon-114.png">
    <link rel="apple-touch-icon" sizes="72x72" href="/apple-touch-icon-144.png">
    <link rel="apple-touch-icon" sizes="144x144" href="/apple-touch-icon-144.png">
    <meta property="fb:app_id" content="1401488693436528">

      <meta content="@github" name="twitter:site" /><meta content="summary" name="twitter:card" /><meta content="danlwarren/RWTY" name="twitter:title" /><meta content="RWTY - R We There Yet?" name="twitter:description" /><meta content="https://avatars0.githubusercontent.com/u/3770891?v=3&amp;s=400" name="twitter:image:src" />
      <meta content="GitHub" property="og:site_name" /><meta content="object" property="og:type" /><meta content="https://avatars0.githubusercontent.com/u/3770891?v=3&amp;s=400" property="og:image" /><meta content="danlwarren/RWTY" property="og:title" /><meta content="https://github.com/danlwarren/RWTY" property="og:url" /><meta content="RWTY - R We There Yet?" property="og:description" />
      <meta name="browser-stats-url" content="https://api.github.com/_private/browser/stats">
    <meta name="browser-errors-url" content="https://api.github.com/_private/browser/errors">
    <link rel="assets" href="https://assets-cdn.github.com/">
    <link rel="web-socket" href="wss://live.github.com/_sockets/Mzc3MDg5MTplOGM1ZGU5MGU3ZDUyNzgyZGQxMjQ3NGIyOGFmNjJkMDoxNTNjNGQzNzQ2YTAzMGJmZTQzNTZlNGVhNmJjODhmMGM0YWY1M2ExMTMyNzRiODJmMTZiMzRhMDFhYmJhYTFj--95cd5933cd5bded1c504dad3e6d673d919dcd11c">
    <meta name="pjax-timeout" content="1000">
    <link rel="sudo-modal" href="/sessions/sudo_modal">

    <meta name="msapplication-TileImage" content="/windows-tile.png">
    <meta name="msapplication-TileColor" content="#ffffff">
    <meta name="selected-link" value="repo_source" data-pjax-transient>

    <meta name="google-site-verification" content="KT5gs8h0wvaagLKAVWq8bbeNwnZZK1r1XQysX3xurLU">
        <meta name="google-analytics" content="UA-3769691-2">

    <meta content="collector.githubapp.com" name="octolytics-host" /><meta content="collector-cdn.github.com" name="octolytics-script-host" /><meta content="github" name="octolytics-app-id" /><meta content="7C95A097:0F1E:8F2146C:55EF80B4" name="octolytics-dimension-request_id" /><meta content="3770891" name="octolytics-actor-id" /><meta content="danlwarren" name="octolytics-actor-login" /><meta content="f585d43758f20d2fd3aab3c5b14c5edae21493f8be67cafdf63fd6b4f9d38daa" name="octolytics-actor-hash" />
    
    <meta content="Rails, view, blob#show" data-pjax-transient="true" name="analytics-event" />
    <meta class="js-ga-set" name="dimension1" content="Logged In">
      <meta class="js-ga-set" name="dimension4" content="Current repo nav">
    <meta name="is-dotcom" content="true">
        <meta name="hostname" content="github.com">
    <meta name="user-login" content="danlwarren">

      <link rel="mask-icon" href="https://assets-cdn.github.com/pinned-octocat.svg" color="#4078c0">
      <link rel="icon" type="image/x-icon" href="https://assets-cdn.github.com/favicon.ico">

    <!-- </textarea> --><!-- '"` --><meta content="authenticity_token" name="csrf-param" />
<meta content="qFr0ceDZH/BGdUQhG/VF71mOMr79Cw5+FAg6hfI0cCJcH+kHdn0hc2bYBM3kHRMZMYd13x87MvBIMlhSKprewA==" name="csrf-token" />
    <meta content="65407ba121d0197ecd3953cb5f9c63ae6e3e8b62" name="form-nonce" />

    <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github-297f62b515917a9f8880b23160a0cb5073573d140c9fa1b8c36c54eef056537d.css" media="all" rel="stylesheet" />
    <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github2-ada15c437f3f84dd62f6fa41cd5edd0a378938da05e9ea81b1150c4cd740ebaf.css" media="all" rel="stylesheet" />
    
    


    <meta http-equiv="x-pjax-version" content="84ca538bfa5cf2c854717ee8112ab1d2">

      
  <meta name="description" content="RWTY - R We There Yet?">
  <meta name="go-import" content="github.com/danlwarren/RWTY git https://github.com/danlwarren/RWTY.git">

  <meta content="3770891" name="octolytics-dimension-user_id" /><meta content="danlwarren" name="octolytics-dimension-user_login" /><meta content="12885770" name="octolytics-dimension-repository_id" /><meta content="danlwarren/RWTY" name="octolytics-dimension-repository_nwo" /><meta content="true" name="octolytics-dimension-repository_public" /><meta content="false" name="octolytics-dimension-repository_is_fork" /><meta content="12885770" name="octolytics-dimension-repository_network_root_id" /><meta content="danlwarren/RWTY" name="octolytics-dimension-repository_network_root_nwo" />
  <link href="https://github.com/danlwarren/RWTY/commits/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1.atom" rel="alternate" title="Recent Commits to RWTY:bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1" type="application/atom+xml">

  </head>


  <body class="logged_in  env-production macintosh vis-public page-blob">
    <a href="#start-of-content" tabindex="1" class="accessibility-aid js-skip-to-content">Skip to content</a>

    
    
    



      <div class="header header-logged-in true" role="banner">
  <div class="container clearfix">

    <a class="header-logo-invertocat" href="https://github.com/" data-hotkey="g d" aria-label="Homepage" data-ga-click="Header, go to dashboard, icon:logo">
  <span class="mega-octicon octicon-mark-github"></span>
</a>


      <div class="site-search repo-scope js-site-search" role="search">
          <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/danlwarren/RWTY/search" class="js-site-search-form" data-global-search-url="/search" data-repo-search-url="/danlwarren/RWTY/search" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
  <label class="js-chromeless-input-container form-control">
    <div class="scope-badge">This repository</div>
    <input type="text"
      class="js-site-search-focus js-site-search-field is-clearable chromeless-input"
      data-hotkey="s"
      name="q"
      placeholder="Search"
      aria-label="Search this repository"
      data-global-scope-placeholder="Search GitHub"
      data-repo-scope-placeholder="Search"
      tabindex="1"
      autocapitalize="off">
  </label>
</form>
      </div>

      <ul class="header-nav left" role="navigation">
        <li class="header-nav-item">
          <a href="/pulls" class="js-selected-navigation-item header-nav-link" data-ga-click="Header, click, Nav menu - item:pulls context:user" data-hotkey="g p" data-selected-links="/pulls /pulls/assigned /pulls/mentioned /pulls">
            Pull requests
</a>        </li>
        <li class="header-nav-item">
          <a href="/issues" class="js-selected-navigation-item header-nav-link" data-ga-click="Header, click, Nav menu - item:issues context:user" data-hotkey="g i" data-selected-links="/issues /issues/assigned /issues/mentioned /issues">
            Issues
</a>        </li>
          <li class="header-nav-item">
            <a class="header-nav-link" href="https://gist.github.com/" data-ga-click="Header, go to gist, text:gist">Gist</a>
          </li>
      </ul>

    
<ul class="header-nav user-nav right" id="user-links">
  <li class="header-nav-item">
      <span class="js-socket-channel js-updatable-content"
        data-channel="notification-changed:danlwarren"
        data-url="/notifications/header">
      <a href="/notifications" aria-label="You have unread notifications" class="header-nav-link notification-indicator tooltipped tooltipped-s" data-ga-click="Header, go to notifications, icon:unread" data-hotkey="g n">
          <span class="mail-status unread"></span>
          <span class="octicon octicon-bell"></span>
</a>  </span>

  </li>

  <li class="header-nav-item dropdown js-menu-container">
    <a class="header-nav-link tooltipped tooltipped-s js-menu-target" href="/new"
       aria-label="Create new…"
       data-ga-click="Header, create new, icon:add">
      <span class="octicon octicon-plus left"></span>
      <span class="dropdown-caret"></span>
    </a>

    <div class="dropdown-menu-content js-menu-content">
      <ul class="dropdown-menu dropdown-menu-sw">
        
<a class="dropdown-item" href="/new" data-ga-click="Header, create new repository">
  New repository
</a>


  <a class="dropdown-item" href="/organizations/new" data-ga-click="Header, create new organization">
    New organization
  </a>



  <div class="dropdown-divider"></div>
  <div class="dropdown-header">
    <span title="danlwarren/RWTY">This repository</span>
  </div>
    <a class="dropdown-item" href="/danlwarren/RWTY/issues/new" data-ga-click="Header, create new issue">
      New issue
    </a>
    <a class="dropdown-item" href="/danlwarren/RWTY/settings/collaboration" data-ga-click="Header, create new collaborator">
      New collaborator
    </a>

      </ul>
    </div>
  </li>

  <li class="header-nav-item dropdown js-menu-container">
    <a class="header-nav-link name tooltipped tooltipped-s js-menu-target" href="/danlwarren"
       aria-label="View profile and more"
       data-ga-click="Header, show menu, icon:avatar">
      <img alt="@danlwarren" class="avatar" height="20" src="https://avatars2.githubusercontent.com/u/3770891?v=3&amp;s=40" width="20" />
      <span class="dropdown-caret"></span>
    </a>

    <div class="dropdown-menu-content js-menu-content">
      <div class="dropdown-menu dropdown-menu-sw">
        <div class="dropdown-header header-nav-current-user css-truncate">
          Signed in as <strong class="css-truncate-target">danlwarren</strong>
        </div>
        <div class="dropdown-divider"></div>

        <a class="dropdown-item" href="/danlwarren" data-ga-click="Header, go to profile, text:your profile">
          Your profile
        </a>
        <a class="dropdown-item" href="/stars" data-ga-click="Header, go to starred repos, text:your stars">
          Your stars
        </a>
        <a class="dropdown-item" href="/explore" data-ga-click="Header, go to explore, text:explore">
          Explore
        </a>
        <a class="dropdown-item" href="https://help.github.com" data-ga-click="Header, go to help, text:help">
          Help
        </a>
        <div class="dropdown-divider"></div>

        <a class="dropdown-item" href="/settings/profile" data-ga-click="Header, go to settings, icon:settings">
          Settings
        </a>

        <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/logout" class="logout-form" data-form-nonce="65407ba121d0197ecd3953cb5f9c63ae6e3e8b62" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="bm7PaTQMHOxeWwtBHHaTGdZPRyU8q7BqeAPfUwGGDhFLtVcvAz5ibpGwSWn4cTAkC2mD2MRlP4zCFOosiNzq9g==" /></div>
          <button class="dropdown-item dropdown-signout" data-ga-click="Header, sign out, icon:logout">
            Sign out
          </button>
</form>      </div>
    </div>
  </li>
</ul>


    
  </div>
</div>

      

      


    <div id="start-of-content" class="accessibility-aid"></div>

    <div id="js-flash-container">
</div>


        <div itemscope itemtype="http://schema.org/WebPage">
    <div class="pagehead repohead instapaper_ignore readability-menu">
      <div class="container">

        <div class="clearfix">
          
<ul class="pagehead-actions">

  <li>
      <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/notifications/subscribe" class="js-social-container" data-autosubmit="true" data-form-nonce="65407ba121d0197ecd3953cb5f9c63ae6e3e8b62" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="2hjuVAoRIZJGycGmxFxge93baTC+4vS9sPgnctvg6zYwrJOJja8t4no17lhWC2dpqXLffWmGSuZ/OE9ZjDGiiQ==" /></div>    <input id="repository_id" name="repository_id" type="hidden" value="12885770" />

      <div class="select-menu js-menu-container js-select-menu">
        <a href="/danlwarren/RWTY/subscription"
          class="btn btn-sm btn-with-count select-menu-button js-menu-target" role="button" tabindex="0" aria-haspopup="true"
          data-ga-click="Repository, click Watch settings, action:blob#show">
          <span class="js-select-button">
            <span class="octicon octicon-eye"></span>
            Unwatch
          </span>
        </a>
        <a class="social-count js-social-count" href="/danlwarren/RWTY/watchers">
          12
        </a>

        <div class="select-menu-modal-holder">
          <div class="select-menu-modal subscription-menu-modal js-menu-content" aria-hidden="true">
            <div class="select-menu-header">
              <span class="select-menu-title">Notifications</span>
              <span class="octicon octicon-x js-menu-close" role="button" aria-label="Close"></span>
            </div>

            <div class="select-menu-list js-navigation-container" role="menu">

              <div class="select-menu-item js-navigation-item " role="menuitem" tabindex="0">
                <span class="select-menu-item-icon octicon octicon-check"></span>
                <div class="select-menu-item-text">
                  <input id="do_included" name="do" type="radio" value="included" />
                  <span class="select-menu-item-heading">Not watching</span>
                  <span class="description">Be notified when participating or @mentioned.</span>
                  <span class="js-select-button-text hidden-select-button-text">
                    <span class="octicon octicon-eye"></span>
                    Watch
                  </span>
                </div>
              </div>

              <div class="select-menu-item js-navigation-item selected" role="menuitem" tabindex="0">
                <span class="select-menu-item-icon octicon octicon octicon-check"></span>
                <div class="select-menu-item-text">
                  <input checked="checked" id="do_subscribed" name="do" type="radio" value="subscribed" />
                  <span class="select-menu-item-heading">Watching</span>
                  <span class="description">Be notified of all conversations.</span>
                  <span class="js-select-button-text hidden-select-button-text">
                    <span class="octicon octicon-eye"></span>
                    Unwatch
                  </span>
                </div>
              </div>

              <div class="select-menu-item js-navigation-item " role="menuitem" tabindex="0">
                <span class="select-menu-item-icon octicon octicon-check"></span>
                <div class="select-menu-item-text">
                  <input id="do_ignore" name="do" type="radio" value="ignore" />
                  <span class="select-menu-item-heading">Ignoring</span>
                  <span class="description">Never be notified.</span>
                  <span class="js-select-button-text hidden-select-button-text">
                    <span class="octicon octicon-mute"></span>
                    Stop ignoring
                  </span>
                </div>
              </div>

            </div>

          </div>
        </div>
      </div>
</form>
  </li>

  <li>
    
  <div class="js-toggler-container js-social-container starring-container ">

    <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/danlwarren/RWTY/unstar" class="js-toggler-form starred js-unstar-button" data-form-nonce="65407ba121d0197ecd3953cb5f9c63ae6e3e8b62" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="HZvguYRrXYgmhnaOy0qg0CGpMyEi1t2uyjXbOeR9KVSw6LopW1S02qw9yRsThBuhDk8dyT6kwoLWM/fPSK7THQ==" /></div>
      <button
        class="btn btn-sm btn-with-count js-toggler-target"
        aria-label="Unstar this repository" title="Unstar danlwarren/RWTY"
        data-ga-click="Repository, click unstar button, action:blob#show; text:Unstar">
        <span class="octicon octicon-star"></span>
        Unstar
      </button>
        <a class="social-count js-social-count" href="/danlwarren/RWTY/stargazers">
          6
        </a>
</form>
    <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/danlwarren/RWTY/star" class="js-toggler-form unstarred js-star-button" data-form-nonce="65407ba121d0197ecd3953cb5f9c63ae6e3e8b62" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="RePcKvPBW0vamrfm5OOddHuLXgXmSj13Yz02Rzh7upr+KbsECFPY883bz6spWq/wdTm2o2l3rEODWTZv9NvPVg==" /></div>
      <button
        class="btn btn-sm btn-with-count js-toggler-target"
        aria-label="Star this repository" title="Star danlwarren/RWTY"
        data-ga-click="Repository, click star button, action:blob#show; text:Star">
        <span class="octicon octicon-star"></span>
        Star
      </button>
        <a class="social-count js-social-count" href="/danlwarren/RWTY/stargazers">
          6
        </a>
</form>  </div>

  </li>

        <li>
          <a href="#fork-destination-box" class="btn btn-sm btn-with-count"
              title="Fork your own copy of danlwarren/RWTY to your account"
              aria-label="Fork your own copy of danlwarren/RWTY to your account"
              rel="facebox"
              data-ga-click="Repository, show fork modal, action:blob#show; text:Fork">
            <span class="octicon octicon-repo-forked"></span>
            Fork
          </a>
          <a href="/danlwarren/RWTY/network" class="social-count">4</a>

          <div id="fork-destination-box" style="display: none;">
            <h2 class="facebox-header" data-facebox-id="facebox-header">Where should we fork this repository?</h2>
            <include-fragment src=""
                class="js-fork-select-fragment fork-select-fragment"
                data-url="/danlwarren/RWTY/fork?fragment=1">
              <img alt="Loading" height="64" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-128.gif" width="64" />
            </include-fragment>
          </div>
        </li>

</ul>

          <h1 itemscope itemtype="http://data-vocabulary.org/Breadcrumb" class="entry-title public ">
  <span class="mega-octicon octicon-repo"></span>
  <span class="author"><a href="/danlwarren" class="url fn" itemprop="url" rel="author"><span itemprop="title">danlwarren</span></a></span><!--
--><span class="path-divider">/</span><!--
--><strong><a href="/danlwarren/RWTY" data-pjax="#js-repo-pjax-container">RWTY</a></strong>

  <span class="page-context-loader">
    <img alt="" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
  </span>

</h1>

        </div>
      </div>
    </div>

    <div class="container">
      <div class="repository-with-sidebar repo-container new-discussion-timeline ">
        <div class="repository-sidebar clearfix">
          
<nav class="sunken-menu repo-nav js-repo-nav js-sidenav-container-pjax js-octicon-loaders"
     role="navigation"
     data-pjax="#js-repo-pjax-container"
     data-issue-count-url="/danlwarren/RWTY/issues/counts">
  <ul class="sunken-menu-group">
    <li class="tooltipped tooltipped-w" aria-label="Code">
      <a href="/danlwarren/RWTY" aria-label="Code" aria-selected="true" class="js-selected-navigation-item selected sunken-menu-item" data-hotkey="g c" data-selected-links="repo_source repo_downloads repo_commits repo_releases repo_tags repo_branches /danlwarren/RWTY">
        <span class="octicon octicon-code"></span> <span class="full-word">Code</span>
        <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>    </li>

      <li class="tooltipped tooltipped-w" aria-label="Issues">
        <a href="/danlwarren/RWTY/issues" aria-label="Issues" class="js-selected-navigation-item sunken-menu-item" data-hotkey="g i" data-selected-links="repo_issues repo_labels repo_milestones /danlwarren/RWTY/issues">
          <span class="octicon octicon-issue-opened"></span> <span class="full-word">Issues</span>
          <span class="js-issue-replace-counter"></span>
          <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>

    <li class="tooltipped tooltipped-w" aria-label="Pull requests">
      <a href="/danlwarren/RWTY/pulls" aria-label="Pull requests" class="js-selected-navigation-item sunken-menu-item" data-hotkey="g p" data-selected-links="repo_pulls /danlwarren/RWTY/pulls">
          <span class="octicon octicon-git-pull-request"></span> <span class="full-word">Pull requests</span>
          <span class="js-pull-replace-counter"></span>
          <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>    </li>

      <li class="tooltipped tooltipped-w" aria-label="Wiki">
        <a href="/danlwarren/RWTY/wiki" aria-label="Wiki" class="js-selected-navigation-item sunken-menu-item" data-hotkey="g w" data-selected-links="repo_wiki /danlwarren/RWTY/wiki">
          <span class="octicon octicon-book"></span> <span class="full-word">Wiki</span>
          <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>
  </ul>
  <div class="sunken-menu-separator"></div>
  <ul class="sunken-menu-group">

    <li class="tooltipped tooltipped-w" aria-label="Pulse">
      <a href="/danlwarren/RWTY/pulse" aria-label="Pulse" class="js-selected-navigation-item sunken-menu-item" data-selected-links="pulse /danlwarren/RWTY/pulse">
        <span class="octicon octicon-pulse"></span> <span class="full-word">Pulse</span>
        <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>    </li>

    <li class="tooltipped tooltipped-w" aria-label="Graphs">
      <a href="/danlwarren/RWTY/graphs" aria-label="Graphs" class="js-selected-navigation-item sunken-menu-item" data-selected-links="repo_graphs repo_contributors /danlwarren/RWTY/graphs">
        <span class="octicon octicon-graph"></span> <span class="full-word">Graphs</span>
        <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>    </li>
  </ul>


    <div class="sunken-menu-separator"></div>
    <ul class="sunken-menu-group">
      <li class="tooltipped tooltipped-w" aria-label="Settings">
        <a href="/danlwarren/RWTY/settings" aria-label="Settings" class="js-selected-navigation-item sunken-menu-item" data-selected-links="repo_settings repo_branch_settings hooks /danlwarren/RWTY/settings">
          <span class="octicon octicon-gear"></span> <span class="full-word">Settings</span>
          <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>
    </ul>
</nav>

            <div class="only-with-full-nav">
                
<div class="js-clone-url clone-url open"
  data-protocol-type="http">
  <h3><span class="text-emphasized">HTTPS</span> clone URL</h3>
  <div class="input-group js-zeroclipboard-container">
    <input type="text" class="input-mini input-monospace js-url-field js-zeroclipboard-target"
           value="https://github.com/danlwarren/RWTY.git" readonly="readonly" aria-label="HTTPS clone URL">
    <span class="input-group-button">
      <button aria-label="Copy to clipboard" class="js-zeroclipboard btn btn-sm zeroclipboard-button tooltipped tooltipped-s" data-copied-hint="Copied!" type="button"><span class="octicon octicon-clippy"></span></button>
    </span>
  </div>
</div>

  
<div class="js-clone-url clone-url "
  data-protocol-type="ssh">
  <h3><span class="text-emphasized">SSH</span> clone URL</h3>
  <div class="input-group js-zeroclipboard-container">
    <input type="text" class="input-mini input-monospace js-url-field js-zeroclipboard-target"
           value="git@github.com:danlwarren/RWTY.git" readonly="readonly" aria-label="SSH clone URL">
    <span class="input-group-button">
      <button aria-label="Copy to clipboard" class="js-zeroclipboard btn btn-sm zeroclipboard-button tooltipped tooltipped-s" data-copied-hint="Copied!" type="button"><span class="octicon octicon-clippy"></span></button>
    </span>
  </div>
</div>

  
<div class="js-clone-url clone-url "
  data-protocol-type="subversion">
  <h3><span class="text-emphasized">Subversion</span> checkout URL</h3>
  <div class="input-group js-zeroclipboard-container">
    <input type="text" class="input-mini input-monospace js-url-field js-zeroclipboard-target"
           value="https://github.com/danlwarren/RWTY" readonly="readonly" aria-label="Subversion checkout URL">
    <span class="input-group-button">
      <button aria-label="Copy to clipboard" class="js-zeroclipboard btn btn-sm zeroclipboard-button tooltipped tooltipped-s" data-copied-hint="Copied!" type="button"><span class="octicon octicon-clippy"></span></button>
    </span>
  </div>
</div>



  <div class="clone-options">You can clone with
    <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/users/set_protocol?protocol_selector=http&amp;protocol_type=push" class="inline-form js-clone-selector-form is-enabled" data-form-nonce="65407ba121d0197ecd3953cb5f9c63ae6e3e8b62" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="zDkatqoFhtF54bo2VRKbyV9Lj9aLCuEob5WDtiu844S9cQdVmDTMcDcbMfC+o7bStXn6UTlpxymgrjYCda/mrA==" /></div><button class="btn-link js-clone-selector" data-protocol="http" type="submit">HTTPS</button></form>, <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/users/set_protocol?protocol_selector=ssh&amp;protocol_type=push" class="inline-form js-clone-selector-form is-enabled" data-form-nonce="65407ba121d0197ecd3953cb5f9c63ae6e3e8b62" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="3F5AVcrEQjYQ+A/810PQeQGel+LjrnNTbtWok0LmY0PKBILoTJshHJZFRrZQEo3ICwZRmdbuEB5+Nyo5yP5NUw==" /></div><button class="btn-link js-clone-selector" data-protocol="ssh" type="submit">SSH</button></form>, or <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/users/set_protocol?protocol_selector=subversion&amp;protocol_type=push" class="inline-form js-clone-selector-form is-enabled" data-form-nonce="65407ba121d0197ecd3953cb5f9c63ae6e3e8b62" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="KYl6tgD96GRe7t+n2s889yD/u9/5C+ZldJZk7UM6FDAop/tPDvx8SmCd/iAm6QreyqrJzMWLnFvhlAbgHa56Bg==" /></div><button class="btn-link js-clone-selector" data-protocol="subversion" type="submit">Subversion</button></form>.
    <a href="https://help.github.com/articles/which-remote-url-should-i-use" class="help tooltipped tooltipped-n" aria-label="Get help on which URL is right for you.">
      <span class="octicon octicon-question"></span>
    </a>
  </div>
    <a href="github-mac://openRepo/https://github.com/danlwarren/RWTY" class="btn btn-sm sidebar-button" title="Save danlwarren/RWTY to your computer and use it in GitHub Desktop." aria-label="Save danlwarren/RWTY to your computer and use it in GitHub Desktop.">
      <span class="octicon octicon-desktop-download"></span>
      Clone in Desktop
    </a>

              <a href="/danlwarren/RWTY/archive/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1.zip"
                 class="btn btn-sm sidebar-button"
                 aria-label="Download the contents of danlwarren/RWTY as a zip file"
                 title="Download the contents of danlwarren/RWTY as a zip file"
                 rel="nofollow">
                <span class="octicon octicon-cloud-download"></span>
                Download ZIP
              </a>
            </div>
        </div>
        <div id="js-repo-pjax-container" class="repository-content context-loader-container" data-pjax-container>

          

<a href="/danlwarren/RWTY/blob/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1/rwty/R/output_plot.R" class="hidden js-permalink-shortcut" data-hotkey="y">Permalink</a>

<!-- blob contrib key: blob_contributors:v21:979ce06d705789d778779c66d97d7c93 -->

  <div class="file-navigation js-zeroclipboard-container">
    
<div class="select-menu js-menu-container js-select-menu left">
  <span class="btn btn-sm select-menu-button js-menu-target css-truncate" data-hotkey="w"
    data-ref=""
    title=""
    role="button" aria-label="Switch branches or tags" tabindex="0" aria-haspopup="true">
    <i>Tree:</i>
    <span class="js-select-button css-truncate-target">bb177097ef</span>
  </span>

  <div class="select-menu-modal-holder js-menu-content js-navigation-container" data-pjax aria-hidden="true">

    <div class="select-menu-modal">
      <div class="select-menu-header">
        <span class="select-menu-title">Switch branches/tags</span>
        <span class="octicon octicon-x js-menu-close" role="button" aria-label="Close"></span>
      </div>

      <div class="select-menu-filters">
        <div class="select-menu-text-filter">
          <input type="text" aria-label="Find or create a branch…" id="context-commitish-filter-field" class="js-filterable-field js-navigation-enable" placeholder="Find or create a branch…">
        </div>
        <div class="select-menu-tabs">
          <ul>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="branches" data-filter-placeholder="Find or create a branch…" class="js-select-menu-tab" role="tab">Branches</a>
            </li>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="tags" data-filter-placeholder="Find a tag…" class="js-select-menu-tab" role="tab">Tags</a>
            </li>
          </ul>
        </div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="branches" role="menu">

        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/danlwarren/RWTY/blob/develop/rwty/R/output_plot.R"
               data-name="develop"
               data-skip-pjax="true"
               rel="nofollow">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <span class="select-menu-item-text css-truncate-target" title="develop">
                develop
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/danlwarren/RWTY/blob/master/rwty/R/output_plot.R"
               data-name="master"
               data-skip-pjax="true"
               rel="nofollow">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <span class="select-menu-item-text css-truncate-target" title="master">
                master
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/danlwarren/RWTY/blob/old_ess/rwty/R/output_plot.R"
               data-name="old_ess"
               data-skip-pjax="true"
               rel="nofollow">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <span class="select-menu-item-text css-truncate-target" title="old_ess">
                old_ess
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/danlwarren/RWTY/blob/treespace.single/rwty/R/output_plot.R"
               data-name="treespace.single"
               data-skip-pjax="true"
               rel="nofollow">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <span class="select-menu-item-text css-truncate-target" title="treespace.single">
                treespace.single
              </span>
            </a>
        </div>

          <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/danlwarren/RWTY/branches" class="js-create-branch select-menu-item select-menu-new-item-form js-navigation-item js-new-item-form" data-form-nonce="65407ba121d0197ecd3953cb5f9c63ae6e3e8b62" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="kZLyLV2CNSkwC9CK3YTyNX5twnUaVvfDdMbUJTruziXP7k9nDMpRa47atILakDTAcqjMRLMMehAp5/hvPrOOLA==" /></div>
            <span class="octicon octicon-git-branch select-menu-item-icon"></span>
            <div class="select-menu-item-text">
              <span class="select-menu-item-heading">Create branch: <span class="js-new-item-name"></span></span>
              <span class="description">from ‘bb17709’</span>
            </div>
            <input type="hidden" name="name" id="name" class="js-new-item-value">
            <input type="hidden" name="branch" id="branch" value="bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1">
            <input type="hidden" name="path" id="path" value="rwty/R/output_plot.R">
</form>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="tags">
        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


        </div>

        <div class="select-menu-no-results">Nothing to show</div>
      </div>

    </div>
  </div>
</div>

    <div class="btn-group right">
      <a href="/danlwarren/RWTY/find/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1"
            class="js-show-file-finder btn btn-sm empty-icon tooltipped tooltipped-nw"
            data-pjax
            data-hotkey="t"
            aria-label="Quickly jump between files">
        <span class="octicon octicon-list-unordered"></span>
      </a>
      <button aria-label="Copy file path to clipboard" class="js-zeroclipboard btn btn-sm zeroclipboard-button tooltipped tooltipped-s" data-copied-hint="Copied!" type="button"><span class="octicon octicon-clippy"></span></button>
    </div>

    <div class="breadcrumb js-zeroclipboard-target">
      <span class="repo-root js-repo-root"><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/danlwarren/RWTY/tree/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1" class="" data-branch="bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1" data-pjax="true" itemscope="url" rel="nofollow"><span itemprop="title">RWTY</span></a></span></span><span class="separator">/</span><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/danlwarren/RWTY/tree/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1/rwty" class="" data-branch="bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1" data-pjax="true" itemscope="url" rel="nofollow"><span itemprop="title">rwty</span></a></span><span class="separator">/</span><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/danlwarren/RWTY/tree/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1/rwty/R" class="" data-branch="bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1" data-pjax="true" itemscope="url" rel="nofollow"><span itemprop="title">R</span></a></span><span class="separator">/</span><strong class="final-path">output_plot.R</strong>
    </div>
  </div>


  <div class="commit file-history-tease">
    <div class="file-history-tease-header">
        <img alt="@NickCrouch" class="avatar" height="24" src="https://avatars1.githubusercontent.com/u/4922130?v=3&amp;s=48" width="24" />
        <span class="author"><a href="/NickCrouch" rel="contributor">NickCrouch</a></span>
        <time datetime="2015-08-26T16:03:09Z" is="relative-time">Aug 26, 2015</time>
        <div class="commit-title">
            <a href="/danlwarren/RWTY/commit/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1" class="message" data-pjax="true" title="add output_plot.R">add output_plot.R</a>
        </div>
    </div>

    <div class="participation">
      <p class="quickstat">
        <a href="#blob_contributors_box" rel="facebox">
          <strong>1</strong>
           contributor
        </a>
      </p>
      
    </div>
    <div id="blob_contributors_box" style="display:none">
      <h2 class="facebox-header" data-facebox-id="facebox-header">Users who have contributed to this file</h2>
      <ul class="facebox-user-list" data-facebox-id="facebox-description">
          <li class="facebox-user-list-item">
            <img alt="@NickCrouch" height="24" src="https://avatars1.githubusercontent.com/u/4922130?v=3&amp;s=48" width="24" />
            <a href="/NickCrouch">NickCrouch</a>
          </li>
      </ul>
    </div>
  </div>

<div class="file">
  <div class="file-header">
    <div class="file-actions">

      <div class="btn-group">
        <a href="/danlwarren/RWTY/raw/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1/rwty/R/output_plot.R" class="btn btn-sm " id="raw-url">Raw</a>
          <a href="/danlwarren/RWTY/blame/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1/rwty/R/output_plot.R" class="btn btn-sm js-update-url-with-hash">Blame</a>
        <a href="/danlwarren/RWTY/commits/bb177097ef2fe94f5a8458c3c6df3ddf8fc9a0f1/rwty/R/output_plot.R" class="btn btn-sm " rel="nofollow">History</a>
      </div>


          <button type="button" class="octicon-btn disabled tooltipped tooltipped-n" aria-label="You must be on a branch to make or propose changes to this file">
            <span class="octicon octicon-pencil"></span>
          </button>

        <button type="button" class="octicon-btn octicon-btn-danger disabled tooltipped tooltipped-n" aria-label="You must be on a branch to make or propose changes to this file">
          <span class="octicon octicon-trashcan"></span>
        </button>
    </div>

    <div class="file-info">
        39 lines (20 sloc)
        <span class="file-info-divider"></span>
      0.747 kB
    </div>
  </div>
  

  <div class="blob-wrapper data type-r">
      <table class="highlight tab-size js-file-line-container" data-tab-size="8">
      <tr>
        <td id="L1" class="blob-num js-line-number" data-line-number="1"></td>
        <td id="LC1" class="blob-code blob-code-inner js-file-line"><span class="pl-c"># assumes a 3 column output</span></td>
      </tr>
      <tr>
        <td id="L2" class="blob-num js-line-number" data-line-number="2"></td>
        <td id="LC2" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L3" class="blob-num js-line-number" data-line-number="3"></td>
        <td id="LC3" class="blob-code blob-code-inner js-file-line">library(<span class="pl-smi">coda</span>)</td>
      </tr>
      <tr>
        <td id="L4" class="blob-num js-line-number" data-line-number="4"></td>
        <td id="LC4" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L5" class="blob-num js-line-number" data-line-number="5"></td>
        <td id="LC5" class="blob-code blob-code-inner js-file-line"><span class="pl-en">plot.results</span> <span class="pl-k">&lt;-</span> <span class="pl-k">function</span>(<span class="pl-smi">results</span>){</td>
      </tr>
      <tr>
        <td id="L6" class="blob-num js-line-number" data-line-number="6"></td>
        <td id="LC6" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L7" class="blob-num js-line-number" data-line-number="7"></td>
        <td id="LC7" class="blob-code blob-code-inner js-file-line"><span class="pl-c">## Get info ##</span></td>
      </tr>
      <tr>
        <td id="L8" class="blob-num js-line-number" data-line-number="8"></td>
        <td id="LC8" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L9" class="blob-num js-line-number" data-line-number="9"></td>
        <td id="LC9" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">mcmc.res</span> <span class="pl-k">&lt;-</span> as.mcmc(<span class="pl-smi">data</span><span class="pl-k">$</span><span class="pl-smi">Number.Unique.Sister.Taxa</span>)</td>
      </tr>
      <tr>
        <td id="L10" class="blob-num js-line-number" data-line-number="10"></td>
        <td id="LC10" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L11" class="blob-num js-line-number" data-line-number="11"></td>
        <td id="LC11" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">hpd.res</span> <span class="pl-k">&lt;-</span> HPDinterval(<span class="pl-smi">mcmc.res</span>)[,<span class="pl-c1">2</span>]</td>
      </tr>
      <tr>
        <td id="L12" class="blob-num js-line-number" data-line-number="12"></td>
        <td id="LC12" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L13" class="blob-num js-line-number" data-line-number="13"></td>
        <td id="LC13" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">dd</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span>[<span class="pl-smi">data</span><span class="pl-k">$</span><span class="pl-smi">Number.Unique.Sister.Taxa</span> <span class="pl-k">&gt;</span> <span class="pl-smi">hpd.res</span>,]</td>
      </tr>
      <tr>
        <td id="L14" class="blob-num js-line-number" data-line-number="14"></td>
        <td id="LC14" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L15" class="blob-num js-line-number" data-line-number="15"></td>
        <td id="LC15" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">dd</span> <span class="pl-k">&lt;-</span>  <span class="pl-smi">dd</span>[order(<span class="pl-smi">dd</span><span class="pl-k">$</span><span class="pl-smi">Number.Unique.Sister.Taxa</span>, <span class="pl-v">decreasing</span><span class="pl-k">=</span><span class="pl-c1">TRUE</span>),]</td>
      </tr>
      <tr>
        <td id="L16" class="blob-num js-line-number" data-line-number="16"></td>
        <td id="LC16" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L17" class="blob-num js-line-number" data-line-number="17"></td>
        <td id="LC17" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L18" class="blob-num js-line-number" data-line-number="18"></td>
        <td id="LC18" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">legend.text</span> <span class="pl-k">&lt;-</span> paste(<span class="pl-smi">dd</span>[,<span class="pl-c1">1</span>],<span class="pl-s"><span class="pl-pds">&quot;</span>:<span class="pl-pds">&quot;</span></span>,<span class="pl-smi">dd</span>[,<span class="pl-c1">2</span>])</td>
      </tr>
      <tr>
        <td id="L19" class="blob-num js-line-number" data-line-number="19"></td>
        <td id="LC19" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L20" class="blob-num js-line-number" data-line-number="20"></td>
        <td id="LC20" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L21" class="blob-num js-line-number" data-line-number="21"></td>
        <td id="LC21" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">title.text</span> <span class="pl-k">&lt;-</span> paste(<span class="pl-s"><span class="pl-pds">&quot;</span>Upper 95% Confidence limit:<span class="pl-pds">&quot;</span></span>,<span class="pl-smi">hpd.res</span>)</td>
      </tr>
      <tr>
        <td id="L22" class="blob-num js-line-number" data-line-number="22"></td>
        <td id="LC22" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L23" class="blob-num js-line-number" data-line-number="23"></td>
        <td id="LC23" class="blob-code blob-code-inner js-file-line"><span class="pl-c">## Plot out ##</span></td>
      </tr>
      <tr>
        <td id="L24" class="blob-num js-line-number" data-line-number="24"></td>
        <td id="LC24" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">l</span><span class="pl-k">&lt;-</span><span class="pl-k">matrix</span>(c(<span class="pl-c1">1</span>,<span class="pl-c1">1</span>,<span class="pl-c1">1</span>,<span class="pl-c1">2</span>,<span class="pl-c1">2</span>), <span class="pl-v">ncol</span><span class="pl-k">=</span><span class="pl-c1">5</span>, <span class="pl-v">nrow</span><span class="pl-k">=</span><span class="pl-c1">1</span>)</td>
      </tr>
      <tr>
        <td id="L25" class="blob-num js-line-number" data-line-number="25"></td>
        <td id="LC25" class="blob-code blob-code-inner js-file-line">layout(<span class="pl-smi">l</span>)</td>
      </tr>
      <tr>
        <td id="L26" class="blob-num js-line-number" data-line-number="26"></td>
        <td id="LC26" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L27" class="blob-num js-line-number" data-line-number="27"></td>
        <td id="LC27" class="blob-code blob-code-inner js-file-line"><span class="pl-c"># Plot 1</span></td>
      </tr>
      <tr>
        <td id="L28" class="blob-num js-line-number" data-line-number="28"></td>
        <td id="LC28" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L29" class="blob-num js-line-number" data-line-number="29"></td>
        <td id="LC29" class="blob-code blob-code-inner js-file-line">plot(seq(<span class="pl-c1">1</span>,nrow(<span class="pl-smi">data</span>),<span class="pl-c1">1</span>), <span class="pl-smi">data</span>[,<span class="pl-c1">2</span>],<span class="pl-v">xlab</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span><span class="pl-pds">&quot;</span></span>,<span class="pl-v">pch</span><span class="pl-k">=</span><span class="pl-c1">19</span>,<span class="pl-v">col</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>#0000FF80<span class="pl-pds">&quot;</span></span> ,<span class="pl-v">ylab</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>Number of Unique Taxa<span class="pl-pds">&quot;</span></span>, <span class="pl-v">main</span><span class="pl-k">=</span><span class="pl-smi">title.text</span>)</td>
      </tr>
      <tr>
        <td id="L30" class="blob-num js-line-number" data-line-number="30"></td>
        <td id="LC30" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L31" class="blob-num js-line-number" data-line-number="31"></td>
        <td id="LC31" class="blob-code blob-code-inner js-file-line">abline(<span class="pl-v">h</span><span class="pl-k">=</span><span class="pl-smi">hpd.res</span>, <span class="pl-v">lty</span><span class="pl-k">=</span><span class="pl-c1">2</span>, <span class="pl-v">lwd</span><span class="pl-k">=</span><span class="pl-c1">2</span>, <span class="pl-v">col</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>red<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L32" class="blob-num js-line-number" data-line-number="32"></td>
        <td id="LC32" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L33" class="blob-num js-line-number" data-line-number="33"></td>
        <td id="LC33" class="blob-code blob-code-inner js-file-line"><span class="pl-c"># Plot 2</span></td>
      </tr>
      <tr>
        <td id="L34" class="blob-num js-line-number" data-line-number="34"></td>
        <td id="LC34" class="blob-code blob-code-inner js-file-line">plot(<span class="pl-c1">1</span>, <span class="pl-v">type</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>n<span class="pl-pds">&quot;</span></span>, <span class="pl-v">axes</span><span class="pl-k">=</span><span class="pl-c1">F</span>, <span class="pl-v">xlab</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span><span class="pl-pds">&quot;</span></span>, <span class="pl-v">ylab</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span><span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L35" class="blob-num js-line-number" data-line-number="35"></td>
        <td id="LC35" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L36" class="blob-num js-line-number" data-line-number="36"></td>
        <td id="LC36" class="blob-code blob-code-inner js-file-line">legend(<span class="pl-s"><span class="pl-pds">&quot;</span>left<span class="pl-pds">&quot;</span></span>,<span class="pl-smi">legend.text</span>,<span class="pl-v">bty</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>n<span class="pl-pds">&quot;</span></span>, <span class="pl-v">cex</span><span class="pl-k">=</span><span class="pl-c1">1.1</span>)</td>
      </tr>
      <tr>
        <td id="L37" class="blob-num js-line-number" data-line-number="37"></td>
        <td id="LC37" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L38" class="blob-num js-line-number" data-line-number="38"></td>
        <td id="LC38" class="blob-code blob-code-inner js-file-line">}</td>
      </tr>
</table>

  </div>

</div>

<a href="#jump-to-line" rel="facebox[.linejump]" data-hotkey="l" style="display:none">Jump to Line</a>
<div id="jump-to-line" style="display:none">
  <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="" class="js-jump-to-line-form" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
    <input class="linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" aria-label="Jump to line" autofocus>
    <button type="submit" class="btn">Go</button>
</form></div>

        </div>
      </div>
      <div class="modal-backdrop"></div>
    </div>
  </div>



      <div class="container">
  <div class="site-footer" role="contentinfo">
    <ul class="site-footer-links right">
        <li><a href="https://status.github.com/" data-ga-click="Footer, go to status, text:status">Status</a></li>
      <li><a href="https://developer.github.com" data-ga-click="Footer, go to api, text:api">API</a></li>
      <li><a href="https://training.github.com" data-ga-click="Footer, go to training, text:training">Training</a></li>
      <li><a href="https://shop.github.com" data-ga-click="Footer, go to shop, text:shop">Shop</a></li>
        <li><a href="https://github.com/blog" data-ga-click="Footer, go to blog, text:blog">Blog</a></li>
        <li><a href="https://github.com/about" data-ga-click="Footer, go to about, text:about">About</a></li>
        <li><a href="https://github.com/pricing" data-ga-click="Footer, go to pricing, text:pricing">Pricing</a></li>

    </ul>

    <a href="https://github.com" aria-label="Homepage">
      <span class="mega-octicon octicon-mark-github" title="GitHub"></span>
</a>
    <ul class="site-footer-links">
      <li>&copy; 2015 <span title="0.07130s from github-fe127-cp1-prd.iad.github.net">GitHub</span>, Inc.</li>
        <li><a href="https://github.com/site/terms" data-ga-click="Footer, go to terms, text:terms">Terms</a></li>
        <li><a href="https://github.com/site/privacy" data-ga-click="Footer, go to privacy, text:privacy">Privacy</a></li>
        <li><a href="https://github.com/security" data-ga-click="Footer, go to security, text:security">Security</a></li>
        <li><a href="https://github.com/contact" data-ga-click="Footer, go to contact, text:contact">Contact</a></li>
        <li><a href="https://help.github.com" data-ga-click="Footer, go to help, text:help">Help</a></li>
    </ul>
  </div>
</div>



    
    

    <div id="ajax-error-message" class="flash flash-error">
      <span class="octicon octicon-alert"></span>
      <button type="button" class="flash-close js-flash-close js-ajax-error-dismiss" aria-label="Dismiss error">
        <span class="octicon octicon-x"></span>
      </button>
      Something went wrong with that request. Please try again.
    </div>


      <script crossorigin="anonymous" src="https://assets-cdn.github.com/assets/frameworks-06e65f5639cc52d1aaada53115a54614b60fa90ab446a673e3e1818df167663b.js"></script>
      <script async="async" crossorigin="anonymous" src="https://assets-cdn.github.com/assets/github-6486b386f2ea8e61e35e6f68b6e5506fa17aed1c385b528ece57088cf14a1459.js"></script>
      
      
    <div class="js-stale-session-flash stale-session-flash flash flash-warn flash-banner hidden">
      <span class="octicon octicon-alert"></span>
      <span class="signed-in-tab-flash">You signed in with another tab or window. <a href="">Reload</a> to refresh your session.</span>
      <span class="signed-out-tab-flash">You signed out in another tab or window. <a href="">Reload</a> to refresh your session.</span>
    </div>
  </body>
</html>

