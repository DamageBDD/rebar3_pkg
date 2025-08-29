-module(rebar_pkg_tmpl).
-export([pkgbuild/0, spec/0, deb_control/0, deb_postinst/0]).

pkgbuild() ->
<<"# Maintainer: {{maintainer}}
pkgname={{app}}
pkgver={{version}}
pkgrel=1
pkgdesc='{{description}}'
arch=('{{arch}}')
url='{{homepage}}'
license=('{{license}}')
depends=()
source=()
sha256sums=()

package() {
  install -d \"$pkgdir{{install_prefix}}/bin\"
  install -m755 {{bin_path}} \"$pkgdir{{install_prefix}}/bin/{{app}}\"
  install -d \"$pkgdir/var/lib/{{app}}\" \"$pkgdir/var/log/{{app}}\" \"$pkgdir/etc/{{app}}\"
}
">>.

spec() ->
<<"
Name:           {{app}}
Version:        {{version}}
Release:        1%{?dist}
Summary:        {{description}}
License:        {{license}}
URL:            {{homepage}}
BuildArch:      {{arch}}

%description
{{description}}

%prep

%build

%install
mkdir -p %{buildroot}{{install_prefix}}/bin
install -m 0755 {{bin_path}} %{buildroot}{{install_prefix}}/bin/{{app}}
mkdir -p %{buildroot}/etc/{{app}} %{buildroot}/var/lib/{{app}} %{buildroot}/var/log/{{app}}

%files
{{install_prefix}}/bin/{{app}}
%dir /etc/{{app}}
%dir /var/lib/{{app}}
%dir /var/log/{{app}}

%changelog
* Tue Aug 27 2025 {{maintainer}} - {{version}}-1
- First packaging
">>.

deb_control() ->
<<"Package: {{app}}
Version: {{version}}
Section: utils
Priority: optional
Architecture: {{arch}}
Maintainer: {{maintainer}}
Homepage: {{homepage}}
Description: {{description}}
">>.

deb_postinst() ->
<<"#!/bin/sh
set -e
# simple postinst hook for {{app}}
# create dirs if dpkg skipped ours for some reason
mkdir -p {{etc_dir}} {{var_dir}} {{log_dir}}
exit 0
">>.
